---
title:                "Sprawdzanie, czy katalog istnieje"
html_title:           "Go: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Sprawdzenie, czy dany folder istnieje, to zasadnicza czynność wykonywana przez programistów. Jest to niezbędne w przypadkach, kiedy nasz kod chce przeczytać plik z folderu lub zapisać do niego, ze szczególnym uwzględnieniem dzielenia danych między różnymi uruchomieniami programu.

## Jak to zrobić:

Za pomocą wbudowanej biblioteki os w Go, możemy łatwo sprawdzić, czy dany katalog istnieje czy nie. Oto prosty przykład:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	_, err := os.Stat("/ścieżka/do/folderu")

	if os.IsNotExist(err) {
		fmt.Println("Folder nie istnieje.")
	} else {
		fmt.Println("Folder istnieje.")
	}
}
```

Gdy uruchomisz ten kod, otrzymasz odpowiedź "Folder nie istnieje." lub "Folder istnieje." w zależności od tego, czy ścieżka, którą podałeś, prowadzi do istniejącego folderu.

## Głębsze zrozumienie:

Funkcja `os.Stat` zwraca informacje o pliku. W przypadku błędu, czyli gdy plik lub folder nie istnieje, zwraca wartość `error`. `os.IsNotExist` to funkcja, która zwróci `true`, jeśli błąd wskazuje, że plik/coś nie istnieje.

Go (wcześniej nazywane Golang), stworzone przez Google, koncentruje się na prostocie i niezawodności. Różne metody były używane do sprawdzania, czy katalog istnieje w różnych językach programowania, ale Go zapewnia jedno z najprostszych rozwiązań.

Jeśli chcesz sprawdzić, czy coś, co może być plikiem lub folderem, istnieje bez wskazania konkretnego typu, ten sam kod działa bez zarzutu. Jeśli jednak chciałbyś sprawdzić tylko foldery, musisz posprawdzać typ pliku, który otrzymujesz z `os.FileInfo`.

## Zobacz także:

1. Dokumentacja Go dla 'os' - https://golang.org/pkg/os/

2. Jak używać pakietu 'os' w Go - https://go.dev/blog/using-go-modules

3. Opis funkcji 'os.Stat' - https://pkg.go.dev/os#Stat