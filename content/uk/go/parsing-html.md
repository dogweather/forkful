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

#Що і чому? 

З парсингом HTML ми отримуємо зміст інтернет-сторінок та можемо з ним працювати в своїх програмах. Це дозволяє нам знайти певні елементи, які нам потрібні, та отримати доступ до їх даних.

#Як робити: 

За допомогою мови програмування Go ми можемо легко парсити HTML. Нижче приведені приклади коду та результатів для декількох популярних пакетів у Go.

### GoQuery: 

```Go
doc, err := goquery.NewDocument("http://example.com")
if err != nil {
    log.Fatal(err)
}

doc.Find("h1").Each(func(i int, s *goquery.Selection) {
    fmt.Println(s.Text())
})

// Output:
// "Example Domain"
```

### Colly: 

```Go
c := colly.NewCollector()

c.OnHTML("h1", func(e *colly.HTMLElement) {
    fmt.Println(e.Text)
})

c.Visit("http://example.com")

// Output:
// "Example Domain"
```

### Gokogiri: 

```Go
doc, _ := html.Parse(resp.Body)
root := gokogiri.ParseHtml(doc)
nodes := root.Search("//h1")

for _, node := range nodes {
    fmt.Println(node.Content())
}

// Output:
// "Example Domain"
```

#Глибинний погляд: 

Парсинг HTML не є новою технологією і використовується вже давно. Раніше програмісти використовували різноманітні інструменти, наприклад, регулярні вирази, для пошуку та обробки HTML-коду. Однак, з впровадженням мови програмування Go і пакетів, які спеціально призначені для парсингу HTML, цей процес став значно простішим та ефективнішим. Існує також багато альтернативних інструментів для парсингу HTML, таких як BeautifulSoup для Python та Jsoup для Java.

У пакеті Go - goquery, за допомогою якого ми зробили приклад у першому розділі, реалізований пошук за допомогою CSS-селекторів, що є більш зручним та інтуїтивно зрозумілим для багатьох програмістів. Кожен пакет має свої переваги та особливості, тому рекомендуємо перевірити кожен з них та вибрати найбільш підходящий для своїх потреб.

#Дивись також: 

- Офіційна документація мови Go: https://golang.org/doc/
- Пакет goquery для парсингу HTML: https://github.com/PuerkitoBio/goquery
- Пакет Colly для парсингу HTML та роботів: https://github.com/gocolly/colly
- Пакет Gokogiri для парсингу XML та HTML за допомогою XPath-та CSS-селекторів: https://github.com/moovweb/gokogiri