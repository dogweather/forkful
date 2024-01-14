---
title:                "Ruby: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför

Att ladda ner en webbsida är en användbar funktion för både webbutvecklare och hobbyprogrammerare. Det gör det möjligt att hämta data från en webbsida och sedan manipulera eller använda den på olika sätt.

## Så här

För att ladda ner en webbsida i Ruby, behöver vi använda oss av ett bibliotek som heter "Net::HTTP". Detta bibliotek gör det möjligt för oss att få åtkomst till webbplatser och hämta data från dem.

Först bör vi initialisera en ny instans av "Net::HTTP" och sedan ange den URL vi vill hämta med hjälp av "uri" metod.

```Ruby
require 'net/http'

url = URI('https://www.examplewebsite.com')

Net::HTTP.get(url)
```

Med dessa två rader kod har vi ställt in och fått åtkomst till vår valda webbplats. Nu kan vi bara "putsa" eller skriva ut resultatet för att se den hämtade webbsidan.

```Ruby
require 'net/http'

url = URI('https://www.examplewebsite.com')

puts Net::HTTP.get(url)
```

## Djupt nedsänkning

Det finns många aspekter av att hämta en webbsida som vi kan utforska djupare. Vi kan t.ex. specifiera vilken typ av metod som ska användas (t.ex. GET eller POST) eller ange olika headeralternativ.

Vi kan också använda "OpenURI" biblioteket för att göra processen lite enklare och mer läsbar:

```Ruby
require 'open-uri'

url = 'https://www.examplewebsite.com'

page = open(url)

puts page.read
```

Vi kan också använda Nokogiri biblioteket för att göra webbsidans data mer lättläst och manipulera den för att få ut specifik information:

```Ruby
require 'open-uri'
require 'nokogiri'

url = 'https://www.examplewebsite.com'

page = Nokogiri::HTML(open(url))

# Här kan vi använda CSS-selektorer för att navigera i webbsidans kod
puts page.css('h1').text # kommer att skriva ut huvudrubriken på sidan
```

## Se även

- [Ruby Net::HTTP library documentation](https://ruby-doc.org/stdlib-2.6.3/libdoc/net/http/rdoc/Net/HTTP.html)
- [OpenURI documentation](https://ruby-doc.org/stdlib-2.6.3/libdoc/open-uri/rdoc/OpenURI.html)
- [Nokogiri documentation](https://nokogiri.org/)