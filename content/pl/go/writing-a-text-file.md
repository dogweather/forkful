---
title:                "Pisanie pliku tekstowego"
html_title:           "Go: Pisanie pliku tekstowego"
simple_title:         "Pisanie pliku tekstowego"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co i po co? 
Pisanie pliku tekstowego to proces, dzięki któremu programista może zapisywać i przechowywać dane w czytelnej formie. Jest to przydatne w przypadku, gdy nie chcemy lub nie możemy przechowywać danych w bazie danych lub innym systemie. 


## Jak to zrobić: 
```Go 
func main() { 
  file, err := os.Create("plik.txt") 
  if err != nil { 
    fmt.Println(err) 
  } 
  defer file.Close() 
  fmt.Fprintln(file, "To jest przykładowy tekst do zapisania w pliku") 
}
```

**Wyjście**: Plik tekstowy "plik.txt" został utworzony, a w nim znajduje się napis "To jest przykładowy tekst do zapisania w pliku".


## Głębsza analiza: 
Pisanie plików tekstowych nie jest niczym nowym w programowaniu. Jednakże technologie i narzędzia wykorzystywane do tego celu ulegają ciągłej zmianie i ulepszaniu. W przeszłości popularną alternatywą dla tworzenia plików tekstowych było korzystanie z baz danych czy arkuszy kalkulacyjnych. Jednak z upowszechnieniem kontenerów i mikroserwisów, pisanie plików stało się prostym i niezawodnym sposobem na przechowywanie danych.

Jeśli chodzi o implementację w języku Go, jest ona bardzo prosta i wykorzystuje funkcję `os.Create()`, która tworzy nowy plik i zwraca wskaźnik do tego pliku. Następnie, korzystając ze standardowej funkcji `fmt`, możemy zapisać wybrany tekst do pliku za pomocą funkcji `Fprintln()`.


## Zobacz także: 
- [Dokumentacja języka Go dotycząca tworzenia plików](https://golang.org/pkg/os/#Create)
- [Zalety i wady pisania plików tekstowych w porównaniu do innych sposobów przechowywania danych](https://www.javatpoint.com/os-create-function-in-golang)