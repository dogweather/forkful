---
title:                "Praca z plikami csv"
html_title:           "Go: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli często musisz pracować z plikami CSV, czy to w celu analizy danych czy też generowania raportów, Go jest idealnym językiem do tego zadania. Jego szybkość, prostota i wbudowane funkcje do obsługi plików CSV sprawiają, że jest to wybór wart rozważenia.

## Jak to zrobić

Poniżej znajdziesz przykładowy kod w języku Go, który pokazuje, jak łatwo można wczytać plik CSV i wykonać na nim operacje.

```Go
// Importowanie biblioteki do obsługi plików CSV
import "encoding/csv"

// Wczytanie pliku CSV
file, err := os.Open("dane.csv")
if err != nil {
  // Obsługa błędów
}

// Użycie funkcji NewReader do utworzenia nowego czytelnika
reader := csv.NewReader(file)

// Ustawienie opcji, jeśli plik zawiera nagłówki kolumn
reader.ReadHeader()

// Pętla przez każdy wiersz w pliku
for {
  // Odczytanie kolejnego wiersza
  record, err := reader.Read()
  if err != nil {
    // Koniec pliku lub błąd
    break
  }

  // Dostęp do wartości w poszczególnych kolumnach
  column1 := record[0]
  column2 := record[1]

  // Wyświetlenie wartości
  fmt.Println(column1, column2)
}
```

Przykładowy plik CSV:

| Imię | Nazwisko | Wiek |
|------|----------|------|
| John  | Smith    | 35   |
| Emily | Jones    | 28   |
| David | Brown    | 41   |

Przykładowy output:

```
John Smith
Emily Jones
David Brown
```

## Deep Dive

Go posiada wbudowaną bibliotekę do obsługi plików CSV, co sprawia, że jest to szybkie i wygodne narzędzie w pracy z tym formatem danych. W niektórych przypadkach, gdy plik CSV jest bardzo duży, można skorzystać z funkcji bufferowania, aby zoptymalizować szybkość działania programu.

Spójrzmy na przykładowy kod, który wykorzystuje funkcję bufferowania:

```Go
// Użycie funkcji NewBuffer do utworzenia nowego bufora
buffer := bufio.NewWriter(file)

// Pętla po wierszach w pliku
for i := 0; i < bufferSize; i++ {
  // Odczytanie kolejnego wiersza
  record, err := reader.Read()
  if err != nil {
    // Koniec pliku lub błąd
    break
  }

  // Zapisanie wiersza do bufora
  buffer.Write(record)
}

// Opróżnienie bufora i zapisanie danych do pliku
buffer.Flush()
```

## Zobacz także

- [Dokumentacja Go do obsługi plików CSV](https://golang.org/pkg/encoding/csv/)
- [Funkcje buforowania w języku Go](https://gobyexample.com/buffers)