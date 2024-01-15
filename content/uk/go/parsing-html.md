---
title:                "Аналізування html"
html_title:           "Go: Аналізування html"
simple_title:         "Аналізування html"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/parsing-html.md"
---

{{< edit_this_page >}}

## Чому

Інтернет-ресурси наповнені величезною кількістю інформації, але більшість з неї є в форматі HTML, що робить її незрозумілою для простого користувача. Це робить парсинг HTML корисним інструментом для отримання потрібних даних з веб-сторінки.

## Як

Щоб розпочати парсити HTML за допомогою мови програмування Go, необхідно встановити пакет "goquery" за допомогою команди ```go get``` і додати його до проекту за допомогою команди ```import```. Потім можна використовувати функції з пакету для отримання потрібної інформації з веб-сторінки.

```Go
import "github.com/PuerkitoBio/goquery"

doc, err := goquery.NewDocument("https://uk.wikipedia.org")
if err != nil {
    log.Fatal(err)
}

doc.Find(".mainheader").Each(func(i int, s *goquery.Selection) {
    fmt.Println(s.Text())
})
```
Вище наведений приклад дозволить отримати назву головного заголовка зі сторінки Української Вікіпедії.

## Аналіз

Для парсингу HTML можна використовувати і інші функції з пакету "goquery", які дозволяють отримувати дані по елементу, його атрибутам та тексту всередині елемента. Також можна використовувати селектори, які дозволяють отримувати дані за певним шаблоном, наприклад всі посилання на сторінці або всі елементи певного класу.

## Див. також

- [Документація пакету "goquery"](https://godoc.org/github.com/PuerkitoBio/goquery)
- [Приклади парсингу HTML з використанням мови Go](https://github.com/PuerkitoBio/goquery/tree/master/_examples)