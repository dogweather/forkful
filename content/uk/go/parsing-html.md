---
title:                "Парсинг HTML"
date:                  2024-01-20T15:32:10.254841-07:00
html_title:           "Arduino: Парсинг HTML"
simple_title:         "Парсинг HTML"

category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/parsing-html.md"
---

{{< edit_this_page >}}

## Що і чому?
Парсинг HTML - це процес аналізу HTML коду для отримання даних чи маніпуляції структурою документа. Програмісти роблять це для веб-скрапінгу, тестування веб-інтерфейсів, або автоматизації веб-взаємодій.

## Як це робити:
Go використовує пакет "golang.org/x/net/html" для парсингу HTML. Ось простий приклад:

```Go
package main

import (
	"fmt"
	"golang.org/x/net/html"
	"net/http"
	"strings"
)

func main() {
	resp, err := http.Get("http://example.com/")
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	z := html.NewTokenizer(resp.Body)

	for {
		tt := z.Next()

		switch tt {
		case html.ErrorToken:
			// Кінець документа, виходимо з циклу
			return
		case html.StartTagToken, html.SelfClosingTagToken:
			token := z.Token()
			if token.Data == "a" {
				for _, attr := range token.Attr {
					if attr.Key == "href" {
						fmt.Println("Знайдене посилання:", attr.Val)
						break
					}
				}
			}
		}
	}
}
```

Видасть список усіх посилань (<a href="...">), знайдених на "http://example.com/".

## Поглиблено:
Парсинг HTML не новий. Головний стандарт (DOM - Document Object Model) існує вже понад десятиліття. У Go, пакет "golang.org/x/net/html" імітує частину цього стандарту. Крім нього, є й інші пакети, наприклад "goquery", який схожий на jQuery. Важливо вибирати інструмент, який відповідає задачі: якщо треба тільки витягнути дані – "goquery" буде кращим. Для більш складних завдань з маніпуляції HTML, "golang.org/x/net/html" дасть більше контролю.

## Дивіться також:
- Документація по воркінгу з HTML в Go: https://pkg.go.dev/golang.org/x/net/html
- goquery, для більшій схожості з jQuery: https://github.com/PuerkitoBio/goquery
- Вступ до веб-скрапінгу з Go: https://blog.golang.org/html-template
- Туторіал по golang.org/x/net/html: https://schier.co/blog/2015/04/26/a-simple-web-scraper-in-go.html
