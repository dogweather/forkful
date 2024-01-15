---
title:                "Sprawdzanie czy istnieje katalog"
html_title:           "Go: Sprawdzanie czy istnieje katalog"
simple_title:         "Sprawdzanie czy istnieje katalog"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Wielu programistów ceni sobie porządek i kontrolę nad swoimi plikami i folderami. Sprawdzenie, czy dany folder istnieje, może zapobiec nieprzewidzianym błędom i warto mieć tę umiejętność w swoim zestawie narzędzi. 

## Jak to zrobić?

W Go istnieje prosty sposób na sprawdzenie istnienia folderu. Wystarczy użyć funkcji `os.Stat()` i przekazać jej ścieżkę do poszukiwanego folderu. Następnie, możemy sprawdzić, czy zwrócony przez funkcję wynik nie jest równy `nil`. Jeśli nie, to oznacza, że folder istnieje. 

```Go
folderPath := "/sciezka/do/folderu"
_, err := os.Stat(folderPath)

if err == nil {
    fmt.Println("Folder istnieje")
} else {
    fmt.Println("Folder nie istnieje")
}
```

Przykładowy output, jeśli folder istnieje:

```
Folder istnieje
```

Jeśli chcemy również sprawdzić, czy folder jest plikiem lub linkiem symbolicznym, możemy użyć funkcji `os.IsDir()`. Przykładowy kod wykorzystujący obie funkcje wyglądałby następująco:

```Go
folderPath := "/sciezka/do/folderu"
fileInfo, err := os.Stat(folderPath)

if err == nil && fileInfo.IsDir() {
    fmt.Println("To jest folder")
} else {
    fmt.Println("To nie jest folder")
}
```

## Deep Dive

Funkcja `os.Stat()` korzysta z systemowej komendy `stat` i zwraca strukturę `os.FileInfo`, która zawiera informacje o pliku lub folderze, m.in. nazwę, rozmiar, czas ostatniej modyfikacji. 
Funkcja `os.IsDir()` jest wygodnym sposobem na sprawdzenie, czy dany plik jest folderem, ponieważ w przypadku gdy przekażemy jej jako argument wynik funkcji `os.Stat()`, nie musimy samodzielnie parsować zwróconej struktury.

## Zobacz też

- [Dokumentacja funkcji os.Stat](https://golang.org/pkg/os/#Stat)
- [Dokumentacja funkcji os.IsDir](https://golang.org/pkg/os/#IsDir)
- [Poradnik o pracy z plikami i folderami w Go](https://tutorialedge.net/golang/go-working-with-files/)