---
title:                "Analysering av html"
html_title:           "Ruby: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/parsing-html.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Parsing av HTML er prosessen med å analysere og dekode HTML-kode for å kunne tolke og manipulere nettsider. Programmere bruker dette for å automatisere oppgaver, som for eksempel å hente ut spesifikk informasjon fra en nettside.

# Hvordan:
For å kunne parse HTML i Ruby, kan du bruke biblioteket 'nokogiri', som er svært populært blant Ruby-programmere. Du kan installere dette ved å kjøre kommandoen ```gem install nokogiri``` i terminalen. Etter installasjon kan du nå parse en nettside ved å følge disse trinnene:

1. Importer biblioteket med ```require 'nokogiri'```
2. Bruk metoden ```Nokogiri::HTML()``` for å lese inn HTML-koden fra en nettside som en streng.
3. Du kan deretter bruke CSS-selektorer eller XPath til å hente ut ønsket element fra HTML-dokumentet. For eksempel kan du bruke ```css``` metoden for å finne alle «p»-elementene på en nettside: ```doc.css('p')```
4. Du kan også bruke ```xpath``` metoden for å spesifisere et mer nøyaktig stykke av HTML-koden du vil hente ut. For eksempel kan du bruke ```doc.xpath('//h1')``` for å finne alle «h1»-elementene på en nettside.
5. Til slutt kan du bruke metoden ```text``` for å hente ut teksten fra de valgte elementene.

# Dykk dypere:
Parsing av HTML har vært en viktig del av webutvikling siden starten av internett. Første gang det ble implementert var på 90-tallet da nettlesere trengte å kunne tolke HTML-kode for å vise nettsider. Alternativene til å bruke biblioteker som Nokogiri inkluderer å bruke en regexmønstermatch eller bygge en egen parser fra scratch, men disse metodene kan være mer tidkrevende og mindre nøyaktige.

Implementasjonen av parsing i Nokogiri er basert på et parser-bibliotek kalt Libxml2, som er skrevet i C og har raskere performance sammenlignet med andre HTML-parser-biblioteker. Det er også mulig å bruke Nokogiri for å validere HTML-kode og til og med manipulere og generere ny HTML-kode.

# Se også:
1. [Dokumentasjon for Nokogiri](https://www.nokogiri.org/)
2. [Tutorial for å komme i gang med Nokogiri](https://www.rubyguides.com/2018/10/parsing-html-in-ruby/)
3. [Libxml2 biblioteket](https://www.xmlsoft.org/html/index.html)