---
title:                "Читання текстового файлу"
date:                  2024-01-20T17:54:25.154310-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання текстового файлу"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що і чому?
Читання текстового файлу — це процес зчитування даних з файлу, що зберігає текст. Програмісти роблять це, щоб отримати дані, розібрати информацію, або ж просто вивести текст для користувача.

## Як це зробити:
Злідкуйте за цим кодом, і ви зможете читати файл у вашій Go програмі легко.

```Go
package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	file, err := os.Open("example.txt") // змініть назву файлу на потрібну
	if err != nil {
		log.Fatalf("failed to open file: %s", err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		fmt.Println(scanner.Text()) // виводяться рядки з файла
	}

	if err := scanner.Err(); err != nil {
		log.Fatalf("scanner error: %s", err)
	}
}
```
Прикладний вивід за умови, що `example.txt` містить:
```
Hello, World!
Привіт, Україно!
```
Буде:
```
Hello, World!
Привіт, Україно!
```

## Занурення в тему:
Історично, читання файлів — це одна з основних операцій, яку повинне підтримувати кожне програмне забезпечення. Go, як мова з сучасними особливостями, пропонує ряд інструментів для цього: від низькорівневої роботи з `os` пакетом до високорівневих читачів укладених у `bufio` та `ioutil`.

Альтернативою `bufio.Scanner` є `ioutil.ReadAll`, яка читає весь файл в пам'ять одразу, але не є ефективною для великих файлів. `bufio.Reader` дає гнучкість з розміром буферу і можливість читати файли за частинами.

Що стосується використання, закривати файл важливо, щоб уникнути витоку ресурсів — `defer file.Close()` виконує цю роль.

## Дивіться також:
- [Документація по пакету os](https://pkg.go.dev/os)
- [Документація по пакету bufio](https://pkg.go.dev/bufio)
- [Стаття про обробку файлів у Go](https://go.dev/doc/effective_go#files)
