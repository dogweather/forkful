---
title:                "Парсинг HTML"
date:                  2024-01-20T15:31:43.439104-07:00
html_title:           "Arduino: Парсинг HTML"
simple_title:         "Парсинг HTML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? / Що і Чому?
Parsing HTML – це процес аналізу структури HTML-кода щоб отримати з нього дані. Програмісти роблять це для автоматизації веб-скрапінгу, тестування веб-сайтів, чи перетворення HTML в інші формати.

## How to: / Як це зробити:
У Gleam, для парсингу HTML ми можемо використати зовнішні бібліотеки, такі як `gleam_html`. Ось приклад:


```gleam
import gleam/html
import gleam/string

pub fn parse_html_example(input: String) -> Result(list(html.Node), Nil) {
  html.parse(input)
}

fn main() {
  let raw_html = "<!DOCTYPE html><html><head><title>My Title</title></head><body><p>Hello, World!</p></body></html>"
  let parsed_html = parse_html_example(string.from_slice(raw_html))

  case parsed_html {
    Ok(nodes) ->
      nodes
        |> list.map(html.text_from_node)
        |> list.filter(option.is_some)
        |> list.map(option.unwrap)
        |> io.debug

    Error(_) ->
      io.debug("Failed to parse HTML.")
  }
}
```

Цей код парсує HTML-рядок і виводить текстовий контент з нього в консоль.

## Deep Dive / Глибше занурення:
Parsing HTML інколи складно через непередбачуваність і невалідність реального HTML. У історичному контексті, браузери розвивалися, щоб обробляти неправильний HTML, але для парсерів це залишається проблемою.

Існує багато альтернатив для парсингу HTML на різних мовах програмування як наприклад BeautifulSoup на Python, Nokogiri в Ruby або AngleSharp на C#. Проте, Gleam, будучи молодшою мовою, має менший вибір, але `gleam_html` пропонує чіткий, функціональний підхід до парсингу в Gleam.

`gleam_html` обробляє HTML використовуючи парсингове дерево, що дозволяє нам працювати з вузлами документа як з даними. Це відрізняється від регулярних виразів, які не є надійним способом для парсингу HTML через його складну вкладену структуру.

## See Also / Дивіться також:
- Документація `gleam_html`: https://hexdocs.pm/gleam_html/
- HTML5 Parsing Algorithm: https://html.spec.whatwg.org/multipage/parsing.html
- Веб-скрапінг з Gleam (блог): https://example.com/web-scraping-with-gleam
- Використання регулярних виразів в Gleam: https://hexdocs.pm/gleam_stdlib/gleam/regex/
