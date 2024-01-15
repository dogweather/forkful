---
title:                "Parsera html"
html_title:           "Ruby: Parsera html"
simple_title:         "Parsera html"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Varför
Att parsa HTML (hyper text markup language) är användbart för att kunna manipulera och extrahera data från webbsidor. Det är särskilt användbart för webbaserat skrapning eller webbautomatisering. Det finns flera bibliotek tillgängliga för detta ändamål, men i denna artikel kommer vi att fokusera på hur man utför HTML-parsing med hjälp av Ruby.

## Hur man gör det
Först och främst måste vi installera ett HTML-parsingsbibliotek för Ruby. Ett populärt val är Nokogiri, som vi kan installera med hjälp av följande kommando:

```Ruby
gem install nokogiri
```

När biblioteket är installerat kan vi börja använda det för att parsa HTML. Här är ett enkelt exempel där vi hämtar titeln på en webbsida:

```Ruby
require 'nokogiri'
require 'open-uri'

url = 'https://www.example.com'
page = Nokogiri::HTML(open(url))
puts page.css('title').text
```

Detta kommer att skriva ut titeln på webbsidan som vi har specificerat, i detta fall "Example Domain". Låt oss ta en titt på vad som händer i koden.

Först importerar vi Nokogiri-biblioteket och öppnar URI (universal resource identifier) till vår webbsida. Sedan använder vi Nokogiri för att skapa en sidstruktur som kan bearbetas. Med hjälp av `page.css` kan vi söka efter alla element som matchar vårt CSS-selectoralster, i detta fall "title", och med `text` kan vi få ut innehållet i den HTML-taggen.

Vi kan också använda Nokogiri för att hämta andra element, extrahera data från tabeller eller formulär och mer.

## Deep Dive
Nokogiri är byggt på libxml2, ett snabbt och effektivt C-bibliotek för XML och HTML-parsing. Detta innebär att Nokogiri är den perfekta lösningen för att bearbeta stora mängder data på ett snabbt och tillförlitligt sätt.

En användbar egenskap hos Nokogiri är dess förmåga att utföra XPath-sökningar på en HTML-sida. XPath är ett sätt att beskriva den exakta vägen till ett visst element på en webbsida. Detta är särskilt användbart när man arbetar med komplexa och djupa strukturer.

Utöver Nokogiri finns det också andra bibliotek tillgängliga för HTML-parsing i Ruby, såsom mechanize och hpricot. Det är värt att undersöka olika alternativ för att hitta det som passar bäst för dina specifika behov.

## Se även
- [Nokogiri Dokumentation](https://nokogiri.org/)
- [Ruby-Doc.org](https://ruby-doc.org/stdlib-2.5.3/libdoc/open-uri/rdoc/OpenURI.html)
- [XPath 2.0-specifikation](https://www.w3.org/TR/xpath20/)