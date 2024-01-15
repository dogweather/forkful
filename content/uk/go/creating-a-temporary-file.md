---
title:                "Створення тимчасового файлу"
html_title:           "Go: Створення тимчасового файлу"
simple_title:         "Створення тимчасового файлу"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Чому

Створення тимчасового файлу є зручним і ефективним способом для зберігання та обробки даних, які потрібні лише на короткий проміжок часу. Він також допоможе уникнути перевантаження комп'ютера довгостроковими файлами.

## Як

Найпростіший спосіб створення тимчасового файлу в ефективній мові програмування Go - використання пакету "ioutil". Нижче наведений приклад коду, який створює тимчасовий файл з вмістом "Hello World!" та виводить його в консоль.

```Go
package main

import (
    "fmt"
    "io/ioutil"
)

func main() {
    content := []byte("Hello World!")
    tempFile, err := ioutil.TempFile("", "example")
    if err != nil {
        fmt.Println(err)
    }

    defer tempFile.Close()

    _, err = tempFile.Write(content)
    if err != nil {
        fmt.Println(err)
    }

    fmt.Println("Temporary file created:", tempFile.Name())
}
```

В результаті виконання цього коду у консолі з'явиться повідомлення про створення файлу з унікальним ім'ям. Цей файл буде зберігатися у тимчасовій папці та автоматично видалиться після завершення роботи програми.

## Глибше погрузження

У пакеті "ioutil" є також функції для створення тимчасових папок та для роботи зі шляхами до файлів. Це дозволяє створювати більш складні структури даних, які можуть бути корисними в багатьох програмах.

## Дивіться також

- [Документація "ioutil" пакету в Go](https://golang.org/pkg/io/ioutil/)
- [Приклади використання тимчасових файлів в Go](https://gobyexample.com/temporary-files)