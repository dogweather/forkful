---
title:                "Analysering av html"
html_title:           "Python: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/parsing-html.md"
---

{{< edit_this_page >}}

# Hva & hvorfor?
Parsing av HTML er en vanlig oppgave for programmerere som ønsker å trekke ut spesifikk informasjon fra nettsider. Dette gjøres ved å analysere HTML-koden som utgjør nettsiden og ekstrahere ønsket innhold.

En av hovedårsakene til å gjøre parsing av HTML er å automatisere prosesser som ellers ville vært tidkrevende. Dette kan for eksempel være å hente informasjon fra flere nettsider og sammenstille den i en enkel tabell.

# Hvordan:
```Python
# Importer BeautifulSoup biblioteket
from bs4 import BeautifulSoup

# Definer en variabel med HTML-koden du ønsker å parse
html = "<html><body><h1>Hei, verden!</h1></body></html>"

# Bruk BeautifulSoup til å lage en parser
parser = BeautifulSoup(html, "html.parser")

# Hent ut teksten innenfor <h1> taggen og skriv den ut
print(parser.h1.text)

```
Output: Hei, verden!

# Dypdykk:
Historisk sett har parsing av HTML vært en viktig del av webutvikling og datautvinning. Første versjon av HTML ble utviklet på begynnelsen av 1990-tallet og siden da har versjonsnumrene stadig økt og nye teknologier og standarder har blitt introdusert.

Selv om de fleste programmerere bruker BeautifulSoup til å parse HTML, finnes det også andre alternativer som for eksempel lxml og html5lib bibliotekene. Disse bibliotekene kan være nyttige hvis man har spesielle behov eller ønsker å bruke mer avanserte teknikker for parsing.

Implementeringen av BeautifulSoup er basert på DOM (Document Object Model) trær, som betyr at hver HTML-element blir representert som en node i treet. Dette gjør det enkelt å navigere i koden og hente ut ønsket informasjon.

# Se også:
- [BeautifulSoup biblioteket](https://www.crummy.com/software/BeautifulSoup/)
- [lxml biblioteket](https://lxml.de/)
- [html5lib biblioteket](https://html5lib.readthedocs.io/)