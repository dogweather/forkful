---
title:                "Analyse av HTML"
date:                  2024-01-20T15:33:56.035317-07:00
html_title:           "Arduino: Analyse av HTML"
simple_title:         "Analyse av HTML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing av HTML betyr å lese og forstå HTML-koden slik at vi kan hente ut data eller endre innholdet. Programmerere gjør dette for å skrape nettinnhold, teste nettsider eller automatisere webinteraksjoner.

## Hvordan:
Ruby har flere gems som lar oss parse HTML, men vi fokuserer på Nokogiri, et populært og kraftfullt verktøy.

```Ruby
require 'nokogiri'
require 'open-uri'

# Last ned og parse en HTML-side
doc = Nokogiri::HTML(URI.open('https://example.com'))

# Hent ut alle headingene
headings = doc.css('h1, h2, h3').map(&:text)
puts headings

# Finn et spesifikt element med id
paragraph = doc.at_css('#main-content p').text
puts paragraph

# Søk etter elementer med en klasse
products = doc.css('.product-name').map(&:text)
puts products
```

Forventet output:
```
["Overskrift 1", "Underoverskrift 2", "Seksjonsoverskrift 3"]
"Her er et avsnitt fra hovedinnholdet."
["Produkt 1", "Produkt 2", "Produkt 3"]
```

## Dykk dypere:
Nokogiri er japansk for 'sågøy' og symboliserer gemsens evne til å "så"-gjennom HTML og XML. Det bygger på libxml2, en av de raskeste XML-parsere. Før Nokogiri, brukte Ruby-programmerere REXML, som er ren Ruby, men langsommere. Alternativer til Nokogiri inkluderer Oga og Hpricot, men Nokogiri er ofte foretrukket for sin hastighet og allsidighet.

Parsing av HTML er ikke bare å finne tags; moderne webapplikasjoner bruker JavaScript for å laste innhold dynamisk. Dette krever verktøy som Selenium eller Watir for å kontrollere en nettleser som kjører JavaScript før parsing.

Når det gjelder parsing, er presisjon viktig. Feil håndtering av HTML kan føre til feil data eller manglende informasjon, spesielt med dårlig strukturerte HTML-dokumenter. Nokogiri kan navigere og rense opp selv i komplekse dokumenter, noe som gjør det til et uunnværlig verktøy for web scraping og dataekstraksjon.

## Se Også:
- Nokogiris offisielle nettsted: [http://nokogiri.org](http://nokogiri.org)
- Ruby-Doc for Nokogiri: [https://rubydoc.info/github/sparklemotion/nokogiri](https://rubydoc.info/github/sparklemotion/nokogiri)
- W3C's HTML/XHTML Validator: [https://validator.w3.org](https://validator.w3.org)
- Selenium WebDriver for automatiserte tester og web scraping: [https://www.selenium.dev/documentation/en/webdriver/](https://www.selenium.dev/documentation/en/webdriver/)
