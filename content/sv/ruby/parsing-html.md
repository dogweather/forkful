---
title:                "Analysera html"
html_title:           "Arduino: Analysera html"
simple_title:         "Analysera html"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/parsing-html.md"
---

{{< edit_this_page >}}

# Artikel: Hantering av HTML Parsing i Ruby
---
## Vad & Varför?

HTML-parsing handlar om att omvandla HTML-kod till en mer läsbar och begriplig datastruktur. Programmerare gör det här för att något bära in, strukturera och manipulera webbinnehåll.

## Hur:

I Ruby, vi använder 'Nokogiri' - en ung paket för att tugga på HTML och XML data. Här är ett exempel:

```ruby
require 'nokogiri'
require 'open-uri'

url = 'https://www.example.com'
doc = Nokogiri::HTML(open(url))

doc.xpath('//h1').each do |node|
  puts node.text
end
```
Den här koden snabbar upp alla 'h1' headers från `www.example.com` och skriver ut dem.

## Djup Dykning

`Nokogiri` föddes år 2008, som en Ruby wrapper till 'libxml2' bibliotek, som användes sedan 1998. Som en alternativ, kan du använda 'hpricot', men det är inte lika kraftfull eller aktivt utvecklad.

När du applicerar parsing med Nokogiri händer följande: 

1. Nokogiri öppnar en URL (eller HTML fil).
2. Det genererar en 'DOM' (Document Object Model) träd av HTML data.
3. Sedan du kan utforska, söka eller ändra denna DOM träd anpassade till dina krav.

## Se Också

Nokogiri Dokumentation: https://nokogiri.org/tutorials/parsing_an_html_xml_document.html

HTML & XML: https://developer.mozilla.org/en-US/docs/Web/HTML

Ruby: https://www.ruby-lang.org/sv/