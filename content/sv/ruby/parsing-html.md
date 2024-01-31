---
title:                "Tolka HTML"
date:                  2024-01-20T15:33:25.166506-07:00
simple_title:         "Tolka HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Parsing HTML handlar om att tolka och manipulera HTML-kod så att vi kan interagera med dess innehåll och struktur. Programmerare gör detta för att skrapa webbsidor, automatisera tester av webbapplikationer, och bearbeta information strukturerat.

## Hur man gör:
I Ruby används ofta gemet Nokogiri för att parsa HTML. Det är kraftfullt och enkelt. Här är ett grundläggande exempel:

```ruby
require 'nokogiri'
require 'open-uri'

# Ladda ner och parsa HTML från en webbsida
html = open('https://example.com')
doc = Nokogiri::HTML(html.read)

# Hämta alla h2-taggar
doc.css('h2').each do |h2|
  puts h2.content
end
```
Exempelutskrift kan vara webbsidans h2-rubriker.

## Djupdykning
Nokogiri, släppt 2008, bygger på libxml2 och libxslt, vilka är standardbibliotek för att parsa XML och XSLT. Alternativ till Nokogiri inkluderar Oga och REXML, som är ren Ruby, men Nokogiri är vanligtvis snabbare. När du parsa HTML, är det viktigt att hantera inkonsekvent och "trasig" HTML, vilket Nokogiri hanterar bra med sin felkorrigering.

## Se också:
- Nokogiri-dokumentation: [http://nokogiri.org/](http://nokogiri.org/)
- W3C HTML-parser specifikation: [https://html.spec.whatwg.org/multipage/parsing.html](https://html.spec.whatwg.org/multipage/parsing.html)
