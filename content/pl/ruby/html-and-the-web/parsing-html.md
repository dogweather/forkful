---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:30.336201-07:00
description: "Analiza HTML oznacza roz\u0142o\u017Cenie na czynniki pierwsze kawa\u0142\
  ka kodu HTML, aby zrozumie\u0107 jego struktur\u0119 i zawarto\u015B\u0107. Programi\u015B\
  ci robi\u0105 to w celu wydobycia\u2026"
lastmod: '2024-03-13T22:44:35.930661-06:00'
model: gpt-4-0125-preview
summary: "Analiza HTML oznacza roz\u0142o\u017Cenie na czynniki pierwsze kawa\u0142\
  ka kodu HTML, aby zrozumie\u0107 jego struktur\u0119 i zawarto\u015B\u0107."
title: "Analiza sk\u0142adniowa HTML"
weight: 43
---

## Co i dlaczego?
Analiza HTML oznacza rozłożenie na czynniki pierwsze kawałka kodu HTML, aby zrozumieć jego strukturę i zawartość. Programiści robią to w celu wydobycia danych, manipulacji treścią lub migracji informacji między formatami i systemami.

## Jak to zrobić:
Aby przeprowadzić analizę HTML w Ruby, zainstaluj gem 'Nokogiri' za pomocą `gem install nokogiri`. Nokogiri jest jak scyzoryk szwajcarski do pracy z HTML i XML w Ruby. Oto szybki przykład:

```ruby
require 'nokogiri'
require 'open-uri'

# Wczytanie zawartości HTML ze strony internetowej
html_content = URI.open('http://example.com').read

# Analiza HTML
doc = Nokogiri::HTML(html_content)

# Wydobycie tytułu
title = doc.xpath('//title').text
puts "Tytuł strony to: #{title}"
```

To wypisze coś w stylu: `Tytuł strony to: Domena przykładowa`.

## Pogłębiona analiza
W początkowych dniach Ruby, opcje do analizy HTML były ograniczone. REXML był wbudowany, ale wolny. Potem pojawił się Hpricot, ale szybko zniknął. Nokogiri zadebiutowało w 2008 roku, łącząc łatwość użycia Hpricot z prędkością i mocą libxml, sprawdzonego zestawu narzędzi XML.

W świecie analizy zawsze są alternatywy. Niektórzy przysięgają na wbudowaną bibliotekę 'rexml' lub 'oga', inną przetwornicę XML/HTML dla Ruby. Ale Nokogiri pozostaje ulubionym ze względu na jego solidność i szybkość, nie wspominając o obszernej gamie funkcji.

Pod spodem, Nokogiri konwertuje HTML na Model Obiektowy Dokumentu (DOM) – strukturę drzewiastą. To ułatwia nawigację i manipulowanie elementami. Używając selektorów XPath i CSS, możesz wyznaczyć dowolny kawałek informacji, którego potrzebujesz.

## Zobacz też
- Gem Nokogiri: [https://nokogiri.org/](https://nokogiri.org/)
- Dokumentacja Ruby's rexml: [https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html](https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html)
- Alternatywne narzędzie do analizy 'oga': [https://github.com/YorickPeterse/oga](https://github.com/YorickPeterse/oga)
- Dowiedz się więcej na temat XPath: [https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)
