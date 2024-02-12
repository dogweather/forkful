---
title:                "Analysering av HTML"
aliases:
- /no/bash/parsing-html/
date:                  2024-02-03T19:11:30.519934-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analysering av HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å parse HTML betyr å sile gjennom strukturen og innholdet i en HTML-fil for å trekke ut informasjon. Programmerere gjør det for å få tilgang til data, manipulere innhold, eller skrape nettsteder.

## Hvordan:

Bash er ikke gå-til for parsing av HTML, men det kan gjøres med verktøy som `grep`, `awk`, `sed`, eller eksterne verktøy som `lynx`. For robusthet vil vi bruke `xmllint` fra `libxml2`-pakken.

```bash
# Installer xmllint om nødvendig
sudo apt-get install libxml2-utils

# Eksempel HTML
cat > sample.html <<EOF
<html>
<head>
  <title>Eksempelside</title>
</head>
<body>
  <h1>Hei, Bash!</h1>
  <p id="myPara">Bash kan lese meg.</p>
</body>
</html>
EOF

# Parse tittelen
title=$(xmllint --html --xpath '//title/text()' sample.html 2>/dev/null)
echo "Tittelen er: $title"

# Trekk ut avsnitt ved ID
para=$(xmllint --html --xpath '//*[@id="myPara"]/text()' sample.html 2>/dev/null)
echo "Avsnittet er: $para"
```

Output:
```
Tittelen er: Eksempelside
Avsnittet er: Bash kan lese meg.
```

## Dypdykk

Tilbake i tid brukte programmerere regex-baserte verktøy som `grep` for å skanne HTML, men det var klønete. HTML er ikke regulær – den er kontekstuell. Tradisjonelle verktøy går glipp av dette og kan være utsatt for feil.

Alternativer? Masse. Python med Beautiful Soup, PHP med DOMDocument, JavaScript med DOM-parser – språk med biblioteker designet for å forstå HTMLs struktur.

Å bruke `xmllint` i bash-skript er solid for enkle oppgaver. Det forstår XML, og ved utvidelse, XHTML. Vanlig HTML kan være uforutsigbar, skjønt. Det følger ikke alltid XMLs strenge regler. `xmllint` tvinger HTML inn i en XML-modell som fungerer godt for velformet HTML, men kan snuble på rotete greier.

## Se Også

- [W3Schools - HTML DOM Parser](https://www.w3schools.com/xml/dom_intro.asp): Demystifiserer HTML DOM.
- [MDN Web Docs - Parsing og serialisering av XML](https://developer.mozilla.org/en-US/docs/Web/Guide/Parsing_and_serializing_XML): For XML-parsingsprinsipper som gjelder for XHTML.
- [Beautiful Soup-dokumentasjon](https://www.crummy.com/software/BeautifulSoup/bs4/doc/): Et Python-bibliotek for parsing av HTML.
- [libxml2-dokumentasjon](http://xmlsoft.org/): Detaljer om `xmllint` og relaterte XML-verktøy.
