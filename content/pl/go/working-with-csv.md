---
title:                "Go: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

Pracowanie z plikami CSV może być niezbędne dla wielu projektów i aplikacji w języku Go. CSV (ang. Comma Separated Values) jest popularnym formatem przechowywania i przesyłania danych w postaci tabelarycznej, co czyni go bardzo przydatnym w pracy z dużymi zbiorami danych.

## Jak To Zrobić

Aby pracować z plikami CSV w języku Go, należy wykorzystać pakiet "encoding/csv", który dostarcza wiele funkcji do odczytywania, zapisywania i manipulowania danymi w formacie CSV. Poniżej znajdują się przykładowe kody wykorzystujące ten pakiet:

```Go
file, err := os.Open("dane.csv") //otwarcie pliku csv
if err != nil {
  log.Fatal(err)
}
defer file.Close()

reader := csv.NewReader(file) //utworzenie readera do odczytywania danych
records, err := reader.ReadAll() //odczytanie wszystkich rekordów
if err != nil {
  log.Fatal(err)
}

for _, record := range records {
  //przetworzenie danych
  fmt.Println(record) //wypisanie danych na ekranie
}

writer := csv.NewWriter(os.Stdout) //utworzenie writera do zapisywania danych
data := [][]string{
  {"John", "Doe", "john@example.com"},
  {"Jane", "Smith", "jane@example.com"},
}
writer.WriteAll(data) //zapisanie danych do pliku
```

Powyższy kod odczytuje dane z pliku CSV i wypisuje je na ekranie. Następnie tworzy nowy plik CSV i zapisuje w nim zdefiniowane dane.

## Głębszy Wgląd

Pakiet "encoding/csv" pozwala również na bardziej zaawansowane manipulowanie danymi, takie jak wybieranie konkretnych kolumn, sortowanie czy filtrowanie. Ponadto, umożliwia on też obsługę plików CSV z różnymi separatorami (nie tylko ","), co może być przydatne w niektórych przypadkach.

## Zobacz także

- Dokumentacja pakietu "encoding/csv" w języku Go: https://golang.org/pkg/encoding/csv/
- Przykładowy plik CSV do ćwiczeń: https://github.com/gebeljd/example-csv-file
- Artykuł o polskim blogu o języku Go: https://goblogpl.pl/