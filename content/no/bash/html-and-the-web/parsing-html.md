---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:30.519934-07:00
description: "Hvordan: Bash er ikke g\xE5-til for parsing av HTML, men det kan gj\xF8\
  res med verkt\xF8y som `grep`, `awk`, `sed`, eller eksterne verkt\xF8y som `lynx`.\
  \ For\u2026"
lastmod: '2024-03-13T22:44:40.972325-06:00'
model: gpt-4-0125-preview
summary: "Bash er ikke g\xE5-til for parsing av HTML, men det kan gj\xF8res med verkt\xF8\
  y som `grep`, `awk`, `sed`, eller eksterne verkt\xF8y som `lynx`."
title: Analysering av HTML
weight: 43
---

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
