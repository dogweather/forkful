---
title:                "Go: Скачування веб-сторінки."
simple_title:         "Скачування веб-сторінки."
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Чому

Автоматичне завантаження веб-сторінки може бути корисним в таких випадках, як створення скреперів для збору даних або створення автоматичних тестів для перевірки доступності веб-сайтів. Завантаження веб-сторінок також можна використовувати для отримання інформації про HTML-код сторінки, яке може бути корисним для веб-розробників.

## Як

Для завантаження веб-сторінок у Go, ми можемо використовувати пакет `net/http` та функцію `http.Get()`. Наприклад, ми хочемо завантажити веб-сторінку https://example.com, то ми можемо скористатися таким кодом:

```Go 
resp, err := http.Get("https://example.com")

if err != nil {
    log.Fatal(err)
}

defer resp.Body.Close()

body, err := ioutil.ReadAll(resp.Body)

if err != nil {
    log.Fatal(err)
}

fmt.Println(string(body))
```

В результаті ми отримаємо виведений HTML-код сторінки `https://example.com`.

## Глибока занурення

Крім функції `http.Get()`, ще є багато інших методів та інструментів для завантаження веб-сторінок у Go. Наприклад, пакет `net/http` містить структуру `Client`, яка дає більш гнучкий підхід до завантаження веб-сторінок. Також існують інші пакети, наприклад `golang.org/x/net/html`, які допомагають аналізувати отриманий HTML-код сторінки та отримати потрібні нам дані з нього.

## Дивись також

- [Документація Go для функції http.Get()](https://golang.org/pkg/net/http/#Get)
- [Стаття про завантаження веб-сторінок в Go](https://medium.com/@nate510/http-response-in-golang-89f7ea7c7682)
- [Документація Go для пакету `golang.org/x/net/html`](https://godoc.org/golang.org/x/net/html)