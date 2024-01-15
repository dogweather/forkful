---
title:                "HTML Parsen"
html_title:           "Ruby: HTML Parsen"
simple_title:         "HTML Parsen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Warum

Wenn du jemals mit HTML-Dokumenten zu tun hattest, hast du wahrscheinlich schon herausgefunden, dass es manchmal notwendig ist, die Informationen aus dem Code zu extrahieren. Das Parsing von HTML ist ein häufiger Prozess, der dies ermöglicht und in vielen Anwendungsfällen nützlich ist.

## Wie es geht

Das Parsen von HTML in Ruby ist ziemlich einfach und erfordert nur wenige Zeilen Code. Zuerst müssen wir die Nokogiri-Gem installieren, um das Parsen zu ermöglichen. Dann können wir den folgenden Code verwenden, um eine URL zu öffnen und das HTML zu parsen:

```Ruby
require 'nokogiri'
require 'open-uri'

url = "https://www.example.com"
page = Nokogiri::HTML(open(url))

puts page.css('h1').text
```

Dieses Beispiel verwendet Nokogiri, um die HTML-Struktur der Seite zu analysieren und den Text des ersten h1-Tags auszugeben. Es gibt viele andere Methoden, um Informationen aus dem HTML zu extrahieren, aber die Verwendung von Nokogiri ist eine der einfachsten Möglichkeiten.

## Tieferer Einblick

Das Parsen von HTML kann komplexer werden, wenn die Seite eine komplexere Struktur aufweist oder wenn du spezifische Informationen extrahieren musst. In diesen Fällen ist es hilfreich, die Dokumentation von Nokogiri zu konsultieren, um die verschiedenen Methoden und Optionen zu verstehen, die für das Parsen verfügbar sind.

Ein wichtiger Teil des Parsens von HTML ist das Verständnis von CSS-Selektoren. Diese ermöglichen es dir, die Elemente auf einer Seite präzise auszuwählen und so die gewünschten Informationen herauszufiltern. Es lohnt sich, einige Zeit damit zu verbringen, sich mit den verschiedenen Selektoren vertraut zu machen und zu experimentieren, um die besten Ergebnisse zu erzielen.

## Siehe auch

- [Nokogiri-Dokumentation](https://nokogiri.org/)
- [Eine Einführung in CSS-Selektoren](https://www.w3schools.com/cssref/css_selectors.asp)
- [Weitere Informationen zum Parsen von HTML mit Ruby](https://code.tutsplus.com/tutorials/html-parsing-and-screen-scraping-in-ruby--net-11862)