---
title:                "Створення текстового файлу"
html_title:           "Arduino: Створення текстового файлу"
simple_title:         "Створення текстового файлу"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Що і навіщо?
Запис текстового файлу - це процес збереження даних у файл у читабельному форматі. Програмісти роблять це, щоб зберегти результути роботи, налаштування або конфігураційні дані.

## Як це зробити:
```go
package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	file, err := os.Create("приклад.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	writer := bufio.NewWriter(file)
	_, err = writer.WriteString("Привіт, мої файли!\n")
	if err != nil {
		panic(err)
	}

	err = writer.Flush() // Пам'ятайте очистити буфер!
	if err != nil {
		panic(err)
	}

	fmt.Println("Файл успішно записано.")
}
```
Файл `приклад.txt` містить рядок `Привіт, мої файли!`.

## Пірнання у глибину:
Історично, запис файлів бібліотекою `bufio` в Go дозволяє ефективно обробляти вихідні дані, зменшуючи кількість викликів до файлової системи. Альтернативою є використання `ioutil.WriteFile`, яке здійснює подібні дії, але призначено для випадків, коли потрібно записати файл однією операцією. Детальніше, `'os'` дає більше контролю над тим, як і де створювати файл, тоді як `'bufio'` подає кращі засоби для роботи з потоками даних.

## Дивіться також:
- Офіційний туторіал по роботі з файлами в Go: https://golang.org/doc/articles/wiki/
- Пакет os: https://pkg.go.dev/os
- Пакет bufio: https://pkg.go.dev/bufio