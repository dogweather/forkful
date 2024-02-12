---
title:                "Tolka HTML"
aliases:
- /sv/ruby/parsing-html.md
date:                  2024-02-03T19:13:07.879532-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tolka HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka HTML innebär att bryta ned en bit HTML-kod för att förstå dess struktur och innehåll. Programmerare gör detta för att extrahera data, manipulera innehåll eller migrera information mellan format och system.

## Hur man gör:
För att tolka HTML i Ruby, installera 'Nokogiri'-gem med `gem install nokogiri`. Nokogiri är som en schweizisk armékniv för att arbeta med HTML och XML i Ruby. Här är ett snabbt exempel:

```ruby
require 'nokogiri'
require 'open-uri'

# Ladda in HTML-innehåll från en webbsida
html_content = URI.open('http://example.com').read

# Tolka HTML:en
doc = Nokogiri::HTML(html_content)

# Extrahera titeln
title = doc.xpath('//title').text
puts "Titeln på sidan är: #{title}"
```

Detta ger något i stil med: `Titeln på sidan är: Exempeldomän`.

## Fördjupning
Tillbaka i de tidiga Ruby-dagarna var alternativen för att tolka HTML begränsade. REXML var inbyggt men långsamt. Sedan kom Hpricot, men det svalnade. Nokogiri debuterade 2008, och kombinerade Hpricots enkelhet med snabbheten och kraften hos libxml, ett beprövat XML-verktyg.

I tolkningsvärlden finns det alltid alternativ. Vissa svär vid det inbyggda 'rexml'-biblioteket eller 'oga', en annan XML/HTML-tolk för Ruby. Men Nokogiri förblir en favorit för dess robusthet och snabbhet, för att inte tala om dess omfattande utbud av funktioner.

Under huven omvandlar Nokogiri HTML till ett Document Object Model (DOM) - en trädstruktur. Detta gör det enkelt att navigera och manipulera element. Med hjälp av XPath och CSS-selektorer kan du pinpunkta precis den information du behöver.

## Se även
- Nokogiri-gem: [https://nokogiri.org/](https://nokogiri.org/)
- Rubys rexml-dokumentation: [https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html](https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html)
- Alternativ tolk 'oga': [https://github.com/YorickPeterse/oga](https://github.com/YorickPeterse/oga)
- Lär dig om XPath: [https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)
