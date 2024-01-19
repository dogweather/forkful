---
title:                "Analysering av html"
html_title:           "C#: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing av HTML er prosessen med å oversette HTMLs tekstdokumenter til objekter for programmet å manipulere. Programmerere bruker det fordi det lar dem trekke ut data, manipulere innhold, og dynamisk oppdatere nettsider.

## Hvordan gjøre det:
La oss starte med en enkel oppgave: ekstrahere alle `<a>` tags fra en HTML-fil. `grep` verktøyet med regulære uttrykk kan brukes:

```Bash
$ grep -o '<a[^>]*>.*</a>' file.html
```

Hvis du vil hente attributtene til en tag, kan du bruke `sed`:

```Bash
$ sed -n 's/.*<a href="\([^"]*\).*/\1/p' file.html
```
Resultatet blir nettadressene som `<a>` tags referer til.

For mer komplisert HTML-parsing, kan du bruke biblioteker som pup, xmlstarlet, eller hxselect.

## Dypdykk
HTML-parsing har vært nødvendig siden nettlesere begynte å vise sider skrevet i HTML. Det finnes mange alternative verktøy for parsing av HTML, som bs4 for Python eller Beautiful Soup for Ruby.

Når det gjelder implementering, kan HTML-parsing være komplisert avhengig av kompleksiteten til HTML-dokumentet. En XML-parser kan brukes for enkel HTML, men hvis dokumentet inneholder spesielle karakterer eller ikke-standard syntax, kan det være nødvendig med en spesiell HTML-parser.

## Se Også
For dypere forståelse, besøk disse linkene:

- grep: http://www.gnu.org/software/grep/
- sed: http://www.gnu.org/software/sed/
- pup: https://github.com/ericchiang/pup
- xmlstarlet: http://xmlstar.sourceforge.net/
- hxselect: http://www.html-xml-utils.org/hxselect.1.html