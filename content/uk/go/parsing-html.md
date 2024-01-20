---
title:                "Розбір HTML"
html_title:           "Arduino: Розбір HTML"
simple_title:         "Розбір HTML"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/parsing-html.md"
---

{{< edit_this_page >}}

## Що & Навіщо?

Витяг з HTML - це процес зчитування коду HTML та його перетворення на структуровані дані. Програмісти виконують це для вилучення корисної інформації з веб-сторінок.

## Як це зробити:

Використаємо пакет `GoQuery` в Go для синтаксичного аналізу HTML. Встановіть його командою:

```Go
go get github.com/PuerkitoBio/goquery
```

А потім використайте наступний код:

```Go
package main

import (
	"fmt"
	"github.com/PuerkitoBio/goquery"
	"log"
	"net/http"
)

func scrapWebsite() {
	resp, err := http.Get("http://example.com")
	if err != nil {
		log.Fatal(err)
	}
	defer resp.Body.Close()

	doc, err := goquery.NewDocumentFromReader(resp.Body)
	if err != nil {
		log.Fatal(err)
	}

	doc.Find("div").Each(func(index int, item *goquery.Selection) {
		title := item.Text()
		fmt.Printf("Title %d: '%s'\n", index, title)
	})
}

func main() {
	scrapWebsite()
}
```

Цей код витягує текст з усіх блоків `<div>` з сайту.

## Поглиблений розбір:

1. **Історичний контекст**: Vin Cerf та інші створили мову розмітки HTML у 1980-х році. HTML был першою мовою для опису структури інформації на веб-сторінках. Сьогодні вона є основою всього Всесвітнього вебу.
   
2. **Альтернативи**: Иснують інші способи обробки HTML, залежно від мови програмування. Наприклад, в Python можете використовувати BeautifulSoup, а в Javascript - Cheerio.

3. **Деталі реалізації**: GoQuery написаний на Go і використовує внутрішній пакет Go net / html для синтаксичного аналізу HTML. Це означає, що GoQuery дотримується специфікацій WHATWG, роблячи його надійним варіантом для парсингу HTML.

## Додатково:

1. [Документація GoQuery](https://pkg.go.dev/github.com/PuerkitoBio/goquery)
2. [Розбір HTML у Python за допомогою BeautifulSoup](https://realpython.com/beautiful-soup-web-scraper-python/)
3. [Специфікації HTML WHATWG](https://whatwg.org/)
4. [Вікіпедія про HTML](https://uk.wikipedia.org/wiki/HTML)