---
title:                "Розбір HTML"
date:                  2024-02-03T18:05:56.006998-07:00
model:                 gpt-4-0125-preview
simple_title:         "Розбір HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і Чому?

Парсинг HTML у Go полягає в аналізі вмісту HTML-файлів для вилучення даних, маніпулювання структурою або конвертації HTML у інші формати. Програмісти роблять це для веб-скрапінгу, створення шаблонів та добування даних, використовуючи потужні можливості Go для конкурентної обробки великого обсягу веб-сторінок.

## Як:

Для парсингу HTML у Go ви зазвичай використовуєте пакет `goquery` або стандартний пакет бібліотеки `net/html`. Ось базовий приклад використання `net/html` для вилучення всіх посилань з веб-сторінки:

```go
package main

import (
    "fmt"
    "golang.org/x/net/html"
    "net/http"
)

func main() {
    // Отримати HTML-документ
    res, err := http.Get("http://example.com")
    if err != nil {
        panic(err)
    }
    defer res.Body.Close()

    // Розібрати HTML-документ
    doc, err := html.Parse(res.Body)
    if err != nil {
        panic(err)
    }

    // Функція для рекурсивного обходу DOM
    var f func(*html.Node)
    f = func(n *html.Node) {
        if n.Type == html.ElementNode && n.Data == "a" {
            for _, a := range n.Attr {
                if a.Key == "href" {
                    fmt.Println(a.Val)
                    break
                }
            }
        }
        for c := n.FirstChild; c != nil; c = c.NextSibling {
            f(c)
        }
    }

    // Обійти DOM
    f(doc)
}
```

Приклад виводу (припускаючи, що `http://example.com` містить два посилання):

```
http://www.iana.org/domains/example
http://www.iana.org/domains/reserved
```

Цей код запитує HTML-сторінку, розбирає її та рекурсивно обходить DOM, щоб знайти та вивести атрибути `href` всіх тегів `<a>`.

## Поглиблений Огляд

Пакет `net/html` надає основи для парсингу HTML у Go, безпосередньо реалізуючи алгоритми токенізації та побудови дерева, визначені стандартом HTML5. Цей низькорівневий підхід потужний, але може бути громіздким для складних завдань.

На противагу, додатковий пакет `goquery`, натхненний jQuery, пропонує вищий рівень інтерфейсу, що спрощує маніпуляцію з DOM та обхід. Він дозволяє розробникам писати лаконічний та виразний код для завдань, таких як відбір елементів, вилучення атрибутів та маніпулювання змістом.

Проте, зручність `goquery` йде з ціною додаткової залежності та потенційно повільнішої продуктивності через свій шар абстракції. Вибір між `net/html` та `goquery` (або іншими бібліотеками для парсингу) залежить від конкретних вимог проєкту, таких як потреба в оптимізації продуктивності чи простоті використання.

Історично, парсинг HTML у Go еволюціонував від базових операцій з рядками до витонченої маніпуляції з DOM-деревом, відображаючи зростаючий екосистему мови та попит спільноти на надійні інструменти для веб-скрапінгу та добування даних. Незважаючи на власні можливості, поширеність сторонніх бібліотек, як-от `goquery`, підкреслює перевагу спільноти Go до модульного, повторно використовуваного коду. Однак, для додатків критичної продуктивності програмісти можуть все ще віддавати перевагу пакету `net/html` або навіть вдаватися до регулярних виразів для простих завдань парсингу, маючи на увазі властиві ризики та обмеження парсингу HTML на основі регулярних виразів.