---
title:    "Go: Tworzenie pliku tymczasowego"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie tymczasowych plików jest nieodłączną częścią programowania w języku Go. Często potrzebujemy przechowywać pewne dane tymczasowo, ale nie chcemy pozostawiać ich po zakończeniu działania programu. W takim przypadku tworzenie i korzystanie z tymczasowych plików jest bardzo przydatne.

## Jak to zrobić

Aby utworzyć tymczasowy plik w Go, możemy skorzystać z funkcji `ioutil.TempFile`, która przyjmuje dwa argumenty: katalog, w którym ma zostać utworzony plik tymczasowy, oraz prefiks, który będzie dodany do nazwy pliku tymczasowego. Poniższy przykład kodu tworzy tymczasowy plik w katalogu /tmp o nazwie "temp_", a następnie zapisuje w nim tekst "To jest tymczasowy plik".

```Go
package main

import (
	"fmt"
	"io/ioutil"
)

func main() {
	// Tworzenie tymczasowego pliku
	file, err := ioutil.TempFile("/tmp", "temp_")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	// Zapisanie tekstu w tymczasowym pliku
	fmt.Fprintln(file, "To jest tymczasowy plik")
}
```

W wyniku wykonania powyższego kodu w katalogu /tmp zostanie utworzony tymczasowy plik o nazwie "temp_891118176", a jego zawartością będzie "To jest tymczasowy plik".

## Wnikliwa analiza

Jeśli chcemy bardziej szczegółowo poznać proces tworzenia tymczasowego pliku w Go, warto wiedzieć, że funkcja `ioutil.TempFile` korzysta z funkcji `os.Create` oraz `os.Mkdir`. Dodatkowo, aby upewnić się, że nazwa pliku tymczasowego jest unikalna, funkcja `ioutil.TempFile` dodaje do prefiksu również losowo wygenerowany ciąg znaków.

## Zobacz także

- Dokumentacja funkcji `ioutil.TempFile`: https://golang.org/pkg/io/ioutil/#TempFile
- Omówienie tworzenia tymczasowych plików w języku Go na stronie bloga "The Go Blog": https://blog.golang.org/defer-panic-and-recover
- Przykład wykorzystania tymczasowych plików w projekcie: https://github.com/golang/go/wiki/Lesson-1.5#temporary-files