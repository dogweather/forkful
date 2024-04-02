---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:11.861189-07:00
description: "Parsing av HTML handler om \xE5 trekke ut data eller informasjon fra\
  \ HTML-innhold, en vanlig oppgave n\xE5r man h\xE5ndterer webdata. Programmerere\
  \ gj\xF8r dette for\u2026"
lastmod: '2024-03-13T22:44:41.224770-06:00'
model: gpt-4-0125-preview
summary: "Parsing av HTML handler om \xE5 trekke ut data eller informasjon fra HTML-innhold,\
  \ en vanlig oppgave n\xE5r man h\xE5ndterer webdata. Programmerere gj\xF8r dette\
  \ for\u2026"
title: Analysering av HTML
weight: 43
---

## Hva og hvorfor?

Parsing av HTML handler om å trekke ut data eller informasjon fra HTML-innhold, en vanlig oppgave når man håndterer webdata. Programmerere gjør dette for å automatisere utvinningen av informasjon fra nettsteder, for oppgaver som web scraping, datautvinning, eller automatisert testing.

## Hvordan:

Fish shell er, fremfor alt, ikke designet for direkte parsing av HTML. Det utmerker seg imidlertid ved å lime sammen Unix-verktøy som `curl`, `grep`, `sed`, `awk`, eller ved å bruke spesialiserte verktøy som `pup` eller `beautifulsoup` i et Python-script. Nedenfor er eksempler som viser hvordan du kan utnytte disse verktøyene innenfor Fish shell for å parse HTML.

### Bruke `curl` og `grep`:
Hente HTML-innhold og trekke ut linjer som inneholder lenker:

```fish
curl -s https://example.com | grep -oP '(?<=href=")[^"]*'
```

Utdata:
```
/page1.html
/page2.html
...
```

### Bruke `pup` (et kommandolinjeverktøy for parsing av HTML):

Først, sørg for at `pup` er installert. Deretter kan du bruke det til å trekke ut elementer ved deres tagger, id-er, klasser, osv.

```fish
curl -s https://example.com | pup 'a attr{href}'
```

Utdata, likt `grep`-eksemplet, ville listet href-attributter til `<a>`-taggene.

### Med et Python-script og `beautifulsoup`:

Selv om Fish i seg selv ikke kan parse HTML på en naturlig måte, integreres det sømløst med Python-script. Nedenfor er et kort eksempel som bruker Python med `BeautifulSoup` for å parse og trekke ut titler fra HTML. Pass på at du har `beautifulsoup4` og `requests` installert i ditt Python-miljø.

**parse_html.fish**

```fish
function parse_html -a url
    python -c "
import sys
import requests
from bs4 import BeautifulSoup

response = requests.get(sys.argv[1])
soup = BeautifulSoup(response.text, 'html.parser')

titles = soup.find_all('title')

for title in titles:
    print(title.get_text())
" $url
end
```

Bruk:

```fish
parse_html 'https://example.com'
```

Utdata:
```
Eksempeldomene
```

Hver av disse metodene tjener forskjellige bruksområder og kompleksitetsnivåer, fra enkel tekstmanipulering på kommandolinjen til den fulle parsingkraften av `beautifulsoup` i Python-skript. Avhengig av dine behov og kompleksiteten til HTML-strukturen, kan du velge en enkel Unix-pipeline eller en kraftigere skripttilnærming.
