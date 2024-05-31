# Wczytanie niezbędnych bibliotek
library(Hmisc)
library(dplyr)
library(kableExtra)
library(ggpubr)
library(car)
library(dunn.test)
library(FSA)
library(ggplot2)
library(tidyr)
library(gridExtra)

# Wczytanie danych z pliku CSV
dane <- read.csv2("przykladoweDane-Projekt.csv", sep = ";")

# Uzupełnienie brakujących danych wartościami średnimi dla poszczególnych grup
group_names <- unique(dane$grupa)

# Wyodrębnienie nazw kolumn do uzupełnienia z danymi numerycznymi
columns_to_impute <- colnames(dane)[sapply(dane, is.numeric)]  #sapply oddaje true tam gdzie sa nienumeryczne kolumny
kontrola_data <- with(dane, dane[grupa == group, ])

uzupelnij_puste_srednia <- function(df, cols) {
  for (col in cols) {
    mean_value <- mean(df[[col]], na.rm = TRUE)  
    df[[col]][is.na(df[[col]])] <- mean_value  
  }
  return(df)
}

for (group in group_names) {
  # Wyodrębnienie danych dla bieżącej grupy
  group_data <- dane %>% filter(grupa == group)
  
  # Uzupełnienie brakujących wartości średnimi
  group_data <- uzupelnij_puste_srednia(group_data, columns_to_impute)
  
  # Aktualizacja oryginalnej ramki danych
  dane[dane$grupa == group, ] <- group_data
}

# Funkcja do tworzenia boxplotów i raportowania wartości odstających (z wyłączeniem grupy i płci)
raportuj_wartosci_odstajace <- function(df, cols, grupa_name) {
  cols <- cols[sapply(df[cols], is.numeric)]
  
  # Obliczenie liczby wierszy i kolumn dla układu
  num_cols <- 3
  num_rows <- ceiling(length(cols) / num_cols)
  
  pdf(paste0(grupa_name, "_boxplots.pdf"))  # Zapis boxplotów do pliku PDF
  par(mfrow = c(num_rows, num_cols))  # Dynamiczny układ wykresów
  
  
  outliers_list <- list()  # Lista do gromadzenia wartości odstających
  
  for (col in cols) {
    boxplot(
      df[[col]], 
      main = paste(grupa_name, col), 
      col = "lightskyblue",
      border = "darkblue",
      outcol = "olivedrab",
      whiskcol = "indianred",
      staplecol = "gold2",
      medcol = "limegreen",
      xlab = col,
      ylab = "Values",
      col.axis = "aquamarine3",
      col.main = "tomato2",
      col.lab = "orchid"
    )
    outliers <- boxplot.stats(df[[col]])$out
    if (length(outliers) > 0) {
      outliers_list[[col]] <- outliers
    }
  }
  
  par(mfrow = c(1, 1))  # Resetowanie układu wykresów
  dev.off()  # Zamknięcie pliku PDF
  
  fileConn <- file(paste0(grupa_name, "_outliers.txt"), open = "w")
  for (col in names(outliers_list)) {
    cat("Grupa:", grupa_name, "Kolumna:", col, "Wartości odstające:", paste(outliers_list[[col]], collapse = ", "), "\n", file = fileConn)
  }
  close(fileConn)
}

# Funkcja do zamiany kropek na przecinki w formacie liczbowym
format_decimal <- function(x) {
  gsub("\\.", ",", format(round(x, 2), nsmall = 2))
}

# Przygotowanie funkcji do podsumowania
podsumowanie_kolumn <- function(df, group_col) {
  numeric_cols <- colnames(df)[sapply(df, is.numeric) & colnames(df) != group_col]
  
  df %>%
    group_by_at(group_col) %>%
    summarise(across(all_of(numeric_cols), list(
      count = ~n(),
      min = ~format_decimal(min(.x, na.rm = TRUE)),
      median = ~format_decimal(median(.x, na.rm = TRUE)),
      mean = ~format_decimal(mean(.x, na.rm = TRUE)),
      max = ~format_decimal(max(.x, na.rm = TRUE)),
      sd = ~format_decimal(sd(.x, na.rm = TRUE)),
      IQR = ~format_decimal(IQR(.x, na.rm = TRUE)),
      var = ~format_decimal(var(.x, na.rm = TRUE))
    ), .names = "{col}_{fn}"))
}

# Stosowanie funkcji do danych
podsumowanie_dane <- podsumowanie_kolumn(dane, "grupa")

# Wyświetlanie wyników
print(podsumowanie_dane)

# Zapis do pliku CSV z przecinkami jako separatorem dziesiętnym
write.csv2(podsumowanie_dane, file = "podsumowanie_dane.csv", row.names = FALSE)

for (group in group_names) {
  group_data <- dane %>% filter(grupa == group)
  
  # Tworzenie raportów o wartościach odstających (z wyłączeniem grupy i płci)
  raportuj_wartosci_odstajace(group_data, columns_to_impute, group)
}

# Przygotowanie danych do wykresów słupkowych
mean_data <- podsumowanie_dane %>%
  select(grupa, ends_with("_mean")) %>%
  pivot_longer(-grupa, names_to = "variable", values_to = "mean")

sd_data <- podsumowanie_dane %>%
  select(grupa, ends_with("_sd")) %>%
  pivot_longer(-grupa, names_to = "variable", values_to = "sd")

iqr_data <- podsumowanie_dane %>%
  select(grupa, ends_with("_IQR")) %>%
  pivot_longer(-grupa, names_to = "variable", values_to = "IQR")

var_data <- podsumowanie_dane %>%
  select(grupa, ends_with("_var")) %>%
  pivot_longer(-grupa, names_to = "variable", values_to = "var")

#ŁĄCZNE PORÓWNANIE
# Przygotowanie danych do wykresów pudełkowych
long_data <- dane %>%
  select(grupa, where(is.numeric)) %>%
  pivot_longer(-grupa, names_to = "variable", values_to = "value")


pdf("podsumowanie_wykresy.pdf")

# Tworzenie wykresów słupkowych dla średnich wartości
ggplot(mean_data, aes(x = grupa, y = mean, fill = grupa)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "Średnie wartości dla każdej grupy", x = "Grupa", y = "Średnia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Tworzenie wykresów słupkowych dla odchylenia standardowego
ggplot(sd_data, aes(x = grupa, y = sd, fill = grupa)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "Odchylenie standardowe dla każdej grupy", x = "Grupa", y = "Odchylenie standardowe") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Tworzenie wykresów słupkowych dla IQR
ggplot(iqr_data, aes(x = grupa, y = IQR, fill = grupa)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "Rozstęp międzykwartylowy dla każdej grupy", x = "Grupa", y = "IQR") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Tworzenie wykresów słupkowych dla wariancji
ggplot(var_data, aes(x = grupa, y = var, fill = grupa)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "Wariancja dla każdej grupy", x = "Grupa", y = "Wariancja") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Tworzenie wykresów pudełkowych
ggplot(long_data, aes(x = grupa, y = value, fill = grupa)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "Rozkład wartości dla każdej grupy", x = "Grupa", y = "Wartość") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()



#TRZECI ETAP


# Funkcja do testu Shapiro-Wilka i sprawdzania zgodności z rozkładem normalnym
Shapiro_normalnosc <- function(data, column_name) {
  # Przeprowadzanie testu Shapiro-Wilka dla każdej grupy
  pvalueShapiroTest <- data %>%
    group_by(grupa) %>%
    summarise(
      statistic = shapiro.test(get(column_name))$statistic,
      p.value = shapiro.test(get(column_name))$p.value
    )
  
  # Wyświetlanie wyniku testu
  cat("Test Shapiro-Wilka dla kolumny:", column_name, "\n")
  print(pvalueShapiroTest)
  
  # Sprawdzanie wartości p-value
  zgodnosc <- all(pvalueShapiroTest$p.value > 0.05)
  
  # Zwracanie wyniku
  if (zgodnosc) {
    return("TAK")
  } else {
    return("NIE")
  }
}

# Funkcja do testu jednorodności wariancji Levene'a
Levene_jednorodnosc_wariancji <- function(data, column_name) {
  # Przeprowadzenie testu Levene'a
  formula <- as.formula(paste(column_name, "~ grupa"))
  leveneTestResult <- leveneTest(as.formula(formula), data = data)
  
  # Wyświetlanie wyniku testu
  cat("Test Levene'a dla kolumny:", column_name, "\n")
  print(leveneTestResult)
  
  # Sprawdzenie wartości p-value
  if (leveneTestResult$"Pr(>F)"[1] > 0.05) {
    return("TAK")
  } else {
    return("NIE")
  }
}

# Funkcja Dunn'a
dunn_raport <- function(data, column_name) {
  # Przeprowadzenie testu Dunna
  dunn_test <- dunnTest(data[[column_name]], data$grupa, method = "bh")
  
  # Tworzenie raportu z wynikami testu Dunna
  dunn_results <- as.data.frame(dunn_test$res)
  
  # Filtracja wyników z p < 0.05
  significant_results <- dunn_results %>% filter(P.adj < 0.05)
  
  # Dodanie kolumny z różnicą między 0.05 a wartością p-value
  if (nrow(significant_results) > 0) {
    significant_results <- significant_results %>%
      mutate(istotnosc_roznicy = 0.05 - P.adj,   
             Info = "Istotne statystycznie wartości p-value",
             nazwa_kolumny = column_name)
  }
  
  # Zapisanie wyników do pliku
  report_file <- "dunn_raport.txt"
  
  # Sprawdzenie, czy plik już istnieje
  if (!file.exists(report_file)) {
    # Jeśli plik nie istnieje, dodaj nagłówki
    headers <- c("Comparison", "Z", "P.unadj", "P.adj", "istotnosc_roznicy", "Info", "nazwa_kolumny")
    write.table(significant_results, file = report_file, sep = "\t", quote = FALSE, row.names = FALSE, col.names = headers, append = FALSE)
  } else {
    # Jeśli plik istnieje, dopisz wyniki bez nagłówków
    write.table(significant_results, file = report_file, sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
  }
  
  # Wyświetlanie wyniku testu
  cat("Test Dunna dla kolumny:", column_name, "\n")
  print(significant_results)
  cat("Raport zapisany do:", report_file, "\n")
}

tukeyHSD_raport <- function(data, column_name) {
  # Przeprowadzenie testu Tukeya po analizie wariancji (ANOVA)
  tukey_result <- TukeyHSD(aov(as.formula(paste(column_name, "~ grupa")), data = data))
  
  # Konwersja wyników do ramki danych
  tukey_df <- as.data.frame(tukey_result$`grupa`)
  
  # Filtracja wyników z p < 0.05
  significant_results <- tukey_df %>% filter(`p adj` < 0.05)
  
  # Dodanie kolumny z różnicą między 0.05 a wartością p-value
  if (nrow(significant_results) > 0) {
    significant_results <- significant_results %>%
      mutate(istotnosc_roznicy = 0.05 - `p adj`,
             Info = "Istotne statystycznie wartości p-value",
             porownanie_grup = rownames(significant_results),
             nazwa_kolumny = column_name)
  }
  
  # Przeniesienie kolumny porownanie_grup na pierwsze miejsce
  significant_results <- significant_results %>%
    select(porownanie_grup, nazwa_kolumny, everything())
  
  # Zapisanie wyników do pliku (dopisywanie)
  report_file <- "tukey_report.txt"
  
  if (!file.exists(report_file)) {
    # Jeśli plik nie istnieje, dodaj nagłówki
    headers <- colnames(significant_results)
    write.table(significant_results, file = report_file, sep = "\t", quote = FALSE, row.names = FALSE, col.names = headers, append = FALSE)
  } else {
    # Jeśli plik istnieje, dopisz wyniki bez nagłówków
    write.table(significant_results, file = report_file, sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
  }
  
  # Wyświetlanie wyniku testu
  cat("Test Tukeya dla kolumny:", column_name, "\n")
  print(significant_results)
  cat("Raport zapisany do:", report_file, "\n")
}

Anov_test <- function(data, column_name) {
  # Przeprowadzenie testu ANOVA
  anova_result <- summary(aov(as.formula(paste(column_name, "~ grupa")), data = data))
  
  # Pobranie wartości p-value z wyników
  p_value <- anova_result[[1]][["Pr(>F)"]][[1]]
  
  # Wyświetlenie wyników testu
  cat("Test ANOVA dla kolumny:", column_name, "\n")
  if (p_value < 0.05) {
    cat(p_value, "< 0.05 - są różnice pomiędzy grupami\n")
    tukeyHSD_raport(data, column_name)
  } else {
    cat(p_value, "> 0.05 - brak różnic pomiędzy grupami\n")
  }
  
  # Zwrócenie wyników
}

kruskal_wallis_test <- function(data, column_name) {
  # Przeprowadzenie testu Kruskala-Wallisa
  kruskal_test <- kruskal.test(as.formula(paste(column_name, "~ grupa")), data = data)
  p_value <- kruskal_test$p.value
  
  # Wyświetlanie wyników testu
  cat("Test Kruskala-Wallisa dla kolumny:", column_name, "\n")
  if (p_value < 0.05) {
    cat(p_value, "< 0.05 - są różnice pomiędzy grupami\n")
    dunn_raport(data, column_name)
  } else {
    cat(p_value, "> 0.05 - brak różnic pomiędzy grupami\n")
  }
}

# Iteracja przez wszystkie kolumny
for (col in columns) {
  if (Shapiro_normalnosc(dane, col) == "TAK") {
    if (Levene_jednorodnosc_wariancji(dane, col) == "TAK") {
      Anov_test(dane, col)
    } else {
      kruskal_wallis_test(dane, col)
    }
  } else {
    kruskal_wallis_test(dane, col)
  }
}


# CZWARTY PUNKT 

# Funkcja do testu korelacji
test_korelacji <- function(data, col1, col2, method) {
  result <- cor.test(data[[col1]], data[[col2]], method = method)
  r_value <- result$estimate
  p_value <- result$p.value
  if (r_value > 0) {
    korelacja <- "korelacja dodatnia"
  } else if (r_value == 0) {
    korelacja <- "brak korelacji"
  } else {
    korelacja <- "korelacja ujemna"
  }
  
  if (r_value > 0.7) {
    sila_korelacji <- "bardzo silna korelacja dodatnia"
  } else if (r_value > 0.5) {
    sila_korelacji <- "silna korelacja dodatnia"
  } else if (r_value > 0.3) {
    sila_korelacji <- "korelacja dodatnia o średnim natężeniu"
  } else if (r_value > 0.2) {
    sila_korelacji <- "słaba korelacja dodatnia"
  } else if (r_value > -0.2) {
    sila_korelacji <- "brak korelacji"
  } else if (r_value > -0.3) {
    sila_korelacji <- "słaba korelacja ujemna"
  } else if (r_value > -0.5) {
    sila_korelacji <- "korelacja ujemna o średnim natężeniu"
  } else if (r_value > -0.7) {
    sila_korelacji <- "silna korelacja ujemna"
  } else {
    sila_korelacji <- "bardzo silna korelacja ujemna"
  }
  
  return(list(p_value = p_value, r_value = r_value, korelacja = korelacja, sila_korelacji = sila_korelacji, method = method))
}

# Iteracja przez wszystkie grupy
group_names <- unique(dane$grupa)
cols <- colnames(dane)[sapply(dane, is.numeric)]
results <- data.frame()

pdf("korelacja_wykresy.pdf")

for (group in group_names) {
  group_data <- dane %>% filter(grupa == group)
  
  for (i in 1:(length(cols)-1)) {
    for (j in (i+1):length(cols)) {
      col1 <- cols[i]
      col2 <- cols[j]
      
      if (Shapiro_normalnosc(group_data, col1) == "TAK" && Shapiro_normalnosc(group_data, col2) == "TAK") {
        korelacja_result <- test_korelacji(group_data, col1, col2, method = "pearson")
        
        if (korelacja_result$p_value < 0.05) {
          plot <- ggscatter(group_data, x = col1, y = col2, 
                            add = "reg.line", conf.int = TRUE, 
                            cor.coef = TRUE, cor.method = "pearson",
                            color = "grupa", fill = "grupa",
                            palette = "jco",
                            ylab = paste(col1, "[unit]"), 
                            xlab = paste(col2, "[unit]"))
          
          print(plot)
        }
      } else {
        korelacja_result <- test_korelacji(group_data, col1, col2, method = "spearman")
        #pomijamy regresje liniowa
        if (korelacja_result$p_value < 0.05) {
          plot <- ggscatter(group_data, x = col1, y = col2, 
                            cor.coef = TRUE, cor.method = "spearman",
                            color = "grupa", fill = "grupa",
                            palette = "jco",
                            ylab = paste(col1, "[unit]"), 
                            xlab = paste(col2, "[unit]"))
          
          print(plot)
        }
      }
      
      if (korelacja_result$p_value < 0.05) {
        result_row <- data.frame(
          grupa = group,
          porownywana_para = paste(col1, col2, sep = "-"),
          p_value = korelacja_result$p_value,
          r = korelacja_result$r_value,
          korelacja = korelacja_result$korelacja,
          sila_korelacji = korelacja_result$sila_korelacji,
          metoda = korelacja_result$method
        )
        results <- rbind(results, result_row)
      }
    }
  }
}

dev.off()

write.table(results, file = "korelacja_report.txt", sep = "\t", quote = FALSE, row.names = FALSE)
cat("Raport zapisany do: korelacja_report.txt\n")
