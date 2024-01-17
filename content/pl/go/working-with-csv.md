---
title:                "Praca z plikiem csv"
html_title:           "Go: Praca z plikiem csv"
simple_title:         "Praca z plikiem csv"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Czym jest i dlaczego to robią programiści?

Praca z plikami CSV (zwane również plikami z wartościami oddzielanymi przecinkami) jest częstym zadaniem dla programistów w celu manipulacji i analizowania danych. Pliki CSV są plikami tekstowymi zawierającymi informacje w formacie tabelarycznym, gdzie dane są oddzielone przecinkami. Programiści często pracują z plikami CSV, aby pobierać, przetwarzać i eksportować dane do różnych aplikacji.

## Jak to zrobić?

Go oferuje wiele wbudowanych funkcji i pakietów, które ułatwiają pracę z plikami CSV. Najpierw należy zaimportować pakiet "encoding/csv", aby móc korzystać z funkcji związanych z plikami CSV. Następnie można użyć funkcji "NewReader" lub "NewWriter", aby wczytać lub zapisać dane z pliku CSV.

```Go
package main

import (
  "encoding/csv"
  "fmt"
  "os"
)

func main() {
  // wczytywanie danych z pliku CSV
  file, err := os.Open("dane.csv")
  if err != nil {
  	fmt.Println("Błąd podczas wczytywania pliku CSV:", err)
  	return
  }
  
  reader := csv.NewReader(file)
  records, err := reader.ReadAll()
  if err != nil {
  	fmt.Println("Błąd podczas czytania danych z pliku CSV:", err)
  	return
  }
  
  // wyświetlanie pobranych danych
  for _, row := range records {
  	fmt.Println(row)
  }
  
  // zapisywanie danych do pliku CSV
  data := [][]string{{"Jeden", "1"}, {"Dwa", "2"}, {"Trzy", "3"}}
  
  file, err = os.Create("nowe_dane.csv")
  if err != nil {
    fmt.Println("Błąd podczas tworzenia pliku CSV:", err)
    return
  }
  
  writer := csv.NewWriter(file)
  writer.WriteAll(data)
  writer.Flush()
}
```

W powyższym przykładzie pokazano, jak wczytać dane z istniejącego pliku CSV oraz jak zapisać dane do nowego pliku CSV. W przypadku wczytywania danych, każdy wiersz jest zapisywany jako slice stringów, a następnie można na nim wykonywać operacje. Natomiast przy zapisywaniu danych, należy najpierw utworzyć slice stringów zawierający dane w formacie tabelarycznym, a następnie użyć funkcji "WriteAll" i "Flush".

## Głębsze wgląd

Format CSV został wprowadzony w latach 70. i przez wiele lat był popularnym sposobem przechowywania i przetwarzania danych. Alternatywami dla plików CSV są między innymi formaty JSON i XML, ale format CSV jest nadal wykorzystywany przez wiele aplikacji do wymiany danych.

Pakiet "encoding/csv" oferuje również funkcje do obsługi specjalnych znaków, jak np. cudzysłowy wewnątrz pól lub tabulatory jako separatorów. W przypadku wykonywania operacji na dużych plikach CSV, zaleca się wykorzystanie funkcji z pakietu "bufio" do wczytania danych linia po linii, zamiast używania funkcji "ReadAll".

## Zobacz także

- Dokumentacja pakietu "encoding/csv": https://golang.org/pkg/encoding/csv/
- Poradnik na temat pracy z plikami CSV w Go: https://gobyexample.com/reading-files
- Słowniczek Go: http://www.goworkbook.org/