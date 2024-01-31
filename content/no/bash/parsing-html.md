---
title:                "Analyse av HTML"
date:                  2024-01-20T15:30:07.956066-07:00
simple_title:         "Analyse av HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing HTML betyr å lese og forstå HTML-koden slik at du kan hente ut spesifikk informasjon. Programmere gjør dette for å automatisere prosesser som nettskraping eller for å integrere data fra ulike websider i egne applikasjoner.

## How to:
Bash er ikke ideelt for HTML-parsing, men det kan gjøres med verktøy som `grep`, `sed`, og `awk`. For robust parsing, bruk kommando-linjeverktøy som `lynx`, `pup` eller `xmllint`.

### Eksempel med `grep` og `sed`:
```Bash
echo "<p>Hello, World!</p>" | grep -oP '(?<=<p>).*(?=</p>)'

# Output:
Hello, World!
```

### Eksempel med `pup`:
Installer først `pup` med `brew install pup` eller tilsvarende.
```Bash
echo '<div><p class="message">Hei Verden!</p></div>' | pup 'p.message text{}'

# Output:
Hei Verden!
```

### Eksempel med `xmllint`:
```Bash
echo '<div><p>Hei igjen, Verden!</p></div>' | xmllint --html --xpath '//p/text()' -

# Output:
Hei igjen, Verden!
```

## Deep Dive:
Parsing HTML med Bash er mer av et "hack" enn en ordentlig løsning. Historisk sett har Bash-scripting vært mer fokusert på tekstbehandling. For mer kompleks HTML og ekte parsing er språk som Python med `BeautifulSoup` eller `lxml` mer egnet.

Versjoner av kommandoer som `awk`, `sed`, og `grep` kan strippe enkel HTML, men sliter med kompleks struktur. Verktøy som `pup` og `xmllint` er eksempler på mer hensiktsmessige alternativer som faktisk forstår HTML-strukturen.

Når du parser HTML, er det viktig å huske at HTML-strukturen kan endre seg, så dine script kan fort bli utdaterte. Sikkerhetsaspekter som å håndtere skadelig kode i HTML-en må også vurderes nøye.

## See Also:
- [`pup`](https://github.com/ericchiang/pup) for HTML-parsing i kommandolinjen.
- [`xmllint`](http://xmlsoft.org/xmllint.html) for parsing og validering av XML-filer.
- [`BeautifulSoup`](https://www.crummy.com/software/BeautifulSoup/) for avansert HTML- og XML-parsing i Python.
- Bash-håndboken: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html
