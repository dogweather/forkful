---
title:                "Analyse av HTML"
date:                  2024-01-20T15:31:34.320060-07:00
html_title:           "Arduino: Analyse av HTML"
simple_title:         "Analyse av HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing av HTML betyr å lese og gjøre om koden til et strukturert format som kan manipuleres programmert. Grunnen til at programmerere gjør dette er for å hente eller bearbeide innhold fra websider automatisk.

## Hvordan:
I Fish Shell kan vi bruke verktøy som `pup` for å tolke HTML. Her er hvordan man installerer og bruker `pup`:

```fish
# Installer 'pup', et kommandolinjeverktøy for parsing av HTML 
sudo apt install pup

# Parse en HTML-fil og hent ut alle titler (antatt at du har en fil 'index.html')
cat index.html | pup 'title text{}'
```

Eksempel på utskrift:
```
Din Hjemside Tittel
```

For å hente ut lenker:
```fish
cat index.html | pup 'a attr{href}'
```

Eksempel på utskrift:
```
http://eksempel.com
http://eksempel.com/om
http://eksempel.com/kontakt
```

## Dypdykk:
Fra de første dagene av nettet har det vært behov for å automatisere henting av informasjon fra HTML-dokumenter. Dette startet med enkle skript som grep og sed, men har utviklet seg til mer komplekse verktøy som BeautifulSoup for Python, Nokogiri for Ruby og `pup` for kommandolinjen.

Alternativer til `pup` inkluderer:
- `htmlq`: Ligner på `jq`, men for HTML.
- `xmllint`: Del av libxml2 pakken, mer allsidig men også mer kompleks.

Implementasjonsdetaljer å merke seg:
- HTML er vanligvis ikke så strukturer som XML, og kan være vanskeligere å parse feilfritt.
- Parsing i Fish Shell ved hjelp av rør og andre kommandolinjeverktøy betyr at du behandler HTML som tekst, så det er viktig å håndtere uforutsigbarheter i hvordan HTML er formatert.

## Se Også:
- [pup GitHub side](https://github.com/ericchiang/pup)
- [HTML parsing i Python med BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/)
- [Parsing HTML med Nokogiri og Ruby](https://nokogiri.org/)
