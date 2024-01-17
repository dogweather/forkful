---
title:                "Analiza html"
html_title:           "Ruby: Analiza html"
simple_title:         "Analiza html"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Parsing HTML to czynność polegająca na analizowaniu kodu HTML i wyodrębnianiu z niego potrzebnych informacji. Programiści często używają tej techniki do automatycznego przetwarzania dużej ilości danych, takich jak adresy email czy treści artykułów, zamiast przepisywać je ręcznie.

## Jak:

Przykładowy kod w Ruby do wyparsowania adresów email ze strony internetowej:

```Ruby
require 'nokogiri'
require 'open-uri'

doc = Nokogiri::HTML(URI.open('strona_internetowa'))
emails = doc.css('a[href*="mailto:"]').map { |a| a["href"].gsub("mailto:", "") }
puts emails
```

Output:
```
["example1@gmail.com", "example2@yahoo.com", "example3@hotmail.com"]
```

## Głębsze zagadnienia:

Parsing HTML jest powszechnie używane od początku internetowej ery. Jedną z popularnych bibliotek do tego celu jest Nokogiri, lecz istnieje także wiele innych opcji, takie jak BeautifulSoup czy regular expressions. W celu uzyskania dokładniejszego wyniku, konieczne jest również zrozumienie CSS selectors, dzięki którym można precyzyjnie wybierać interesujące elementy z kodu HTML.

## Zobacz także:

- Dokumentacja Nokogiri: https://nokogiri.org/
- 10 sposobów na parsing HTML w Ruby: https://www.rubyguides.com/10-ways-to-parse-html-in-ruby/