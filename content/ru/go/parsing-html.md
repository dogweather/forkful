---
title:                "Разбор HTML"
date:                  2024-01-28T23:59:58.072801-07:00
model:                 gpt-4-0125-preview
simple_title:         "Разбор HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/go/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Парсинг HTML подразумевает извлечение информации из HTML-файла – это код, лежащий в основе веб-страниц. Программисты делают это для автоматизации получения данных, извлечения содержимого и миграции контента между системами.

## Как это сделать:
У Go есть пакет `net/html`, идеально подходящий для работы с HTML. Вот суть:

```Go
package main

import (
	"fmt"
	"golang.org/x/net/html"
	"net/http"
	"os"
)

func main() {
	// Извлечение HTML
	resp, err := http.Get("http://example.com")
	if err != nil {
		fmt.Fprintf(os.Stderr, "fetch: %v\n", err)
		os.Exit(1)
	}
	defer resp.Body.Close()
	
	// Парсинг HTML
	doc, err := html.Parse(resp.Body)
	if err != nil {
		fmt.Fprintf(os.Stderr, "parse: %v\n", err)
		os.Exit(1)
	}

	// Обход дерева узлов HTML
	var f func(*html.Node)
	f = func(n *html.Node) {
		if n.Type == html.ElementNode && n.Data == "a" {
			for _, a := range n.Attr {
				if a.Key == "href" {
					fmt.Printf("%v\n", a.Val)
				}
			}
		}
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			f(c)
		}
	}
	f(doc)
}
```

Запустите? Вы получите ссылки с `example.com`, выведенные в вашей консоли. Просто!

## Погружение:
Вот что важно знать. Когда интернет только родился, HTML был простым. Но это уже не так. Сегодня он сложен и полон нюансов.

Почему не использовать regex? HTML может быть несогласованным. Использование regex для HTML – это шаткий и склонный к ошибкам подход. Парсеры вроде `net/html` умнее. Они обрабатывают странности и вложенности в HTML, которые сломали бы regex-шаблон.

Парсер `net/html` строит дерево из элементов HTML. Это как придать структуру куче ветвей — превращение хаоса в нечто, по чему можно взбираться. Вы обходите дерево с помощью своих функций, чтобы просеивать теги и атрибуты.

Что еще можно использовать? Библиотеки вроде `goquery` предлагают похожий на jQuery опыт для Go, а `colly` является популярным выбором для скрапинга.

## Смотрите также:
- Пакет `net/html` в Go: https://pkg.go.dev/golang.org/x/net/html
- GoQuery для синтаксиса, похожего на jQuery: https://github.com/PuerkitoBio/goquery
- Colly для скрапинга: http://go-colly.org/
