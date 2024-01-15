---
title:                "Завантаження веб-сторінки"
html_title:           "Go: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Для чого

Веб-сторінки - це основна ресурс для отримання інформації в сучасному світі. Завантаження веб-сторінки дозволяє нам отримати доступ до цієї інформації та обробити її за допомогою програмування Go.

## Як це зробити

```Go
package main

import (
    "fmt"
    "net/http"
    "io/ioutil"
)

func main() {
    response, err := http.Get("https://www.example.com")
    if err != nil {
        fmt.Println("Помилка при завантаженні сторінки!")
    }
    defer response.Body.Close()
    data, err := ioutil.ReadAll(response.Body)
    if err != nil {
        fmt.Println("Помилка при читанні вмісту сторінки!")
    }
    fmt.Println(string(data))
}
```

Результат виконання програми буде виглядати подібно до цього:

```Go
<!DOCTYPE html>
<html>
<head>
    <title>Приклад</title>
</head>
<body>
    <h1>Це приклад веб-сторінки.</h1>
    <p>Вітаємо у нашому світі!</p>
</body>
</html>
```

## Глибока деталь

У програмуванні Go для завантаження веб-сторінки використовується стандартна бібліотека "net/http". Метод "Get" виконує запит до вказаної веб-сторінки та повертає об’єкт "Response", який містить інформацію про сторінку та її вміст. Ми можемо використовувати цей об'єкт для отримання сторінки в форматі байтів або для обробки вмісту сторінки за допомогою функцій бібліотеки "io/ioutil". Уникайте пропущена повернення результатів запиту, перевіряючи наявність помилок та використовуючи інструкцію "defer" для закриття тіла відповіді.

## Дивіться також

- [Офіційна документація зі стандартної бібліотеки Go](https://golang.org/pkg/net/http/)
- [Основи програмування Go](https://www.digitalocean.com/community/tutorial_series/how-to-code-in-go)
- [Ресурси для вивчення Go](https://github.com/golang/go/wiki/Learn)