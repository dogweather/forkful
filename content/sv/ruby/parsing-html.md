---
title:                "Analys av HTML"
html_title:           "Ruby: Analys av HTML"
simple_title:         "Analys av HTML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att parsar HTML är en metod för att extrahera data från en HTML-fil. Detta är användbart för programmerare som vill automatisera processen att få information från en webbsida, till exempel att hämta produkter från en webbutik eller att samla information från nyhetssidor.

## Hur gör man:

```Ruby
require 'nokogiri'
require 'open-uri'

# Hämta en webbsida och spara den som en variabel
webbsida = Nokogiri::HTML(open("https://example.com"))

# Skriv ut all text på sidan
puts webbsida.text

# Hämta alla länkar på sidan och skriv ut dem
länkar = webbsida.css("a")
länkar.each do |länk|
  puts länk["href"]
end
```

## Djupdykning:

Parsning av HTML har funnits sedan webben först utvecklades, och det finns många olika sätt att göra det på. Ett annat alternativ är att använda en parser byggd specifikt för HTML, som [Nokogiri](https://github.com/sparklemotion/nokogiri). Implementationen av en HTML-parser är komplicerad och kräver förståelse för HTML och dess struktur.

## Se även:

- [Nokogiri documentation](https://nokogiri.org/)
- [Ruby on Rails Guides: Working with the Web](https://guides.rubyonrails.org/working_with_javascript_in_rails.html)
- [The 5 Best Ruby HTML Parsers](https://www.rubyguides.com/2016/08/5-best-ruby-html-parsers-examples/)