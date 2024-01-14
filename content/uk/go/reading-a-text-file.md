---
title:                "Go: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Чому

Розуміння як прочитати текстовий файл є важливою навичкою для будь-якого програміста. Це дає можливість працювати зі структурованими даними і зробити роботу з файлами більш ефективною.

## Як

```Go
package main

import (
	"fmt"
	"io/ioutil"
)

func main() {
	data, err := ioutil.ReadFile("test.txt") // зчитати файл
	if err != nil {
		fmt.Println("File reading error", err)
		return
	}

	fmt.Println("Contents of file:", string(data)) // вивід даних файлу
}
```

## Глибокий занурення

У Go є декілька способів читання текстових файлів. Можна використовувати пакет `os` і функцію `Open`, або `bufio` для читання файлів за рядками. Крім того, для роботи з більш великими файлами може знадобитися використовувати `io.Reader` та `io.LimitReader` для зчитування деякої частини файлу.

## Дивись також

- [Офіційна документація зчитування текстових файлів у Go](https://golang.org/pkg/io/ioutil)
- [Огляд пакету `bufio` для читання файлів у Go](https://gobyexample.com/reading-files)
- [Детальна стаття про читання та обробку великих файлів у Go](https://medium.com/@felipedutratine/reading-big-files-in-go-941a0a95df01)