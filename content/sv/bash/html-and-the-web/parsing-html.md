---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:31.909745-07:00
description: "Att parsa HTML inneb\xE4r att g\xE5 igenom strukturen och inneh\xE5\
  llet i en HTML-fil f\xF6r att extrahera information. Programmerare g\xF6r det f\xF6\
  r att komma \xE5t data,\u2026"
lastmod: '2024-03-13T22:44:38.080453-06:00'
model: gpt-4-0125-preview
summary: "Att parsa HTML inneb\xE4r att g\xE5 igenom strukturen och inneh\xE5llet\
  \ i en HTML-fil f\xF6r att extrahera information. Programmerare g\xF6r det f\xF6\
  r att komma \xE5t data,\u2026"
title: Tolka HTML
weight: 43
---

## Vad & Varför?

Att parsa HTML innebär att gå igenom strukturen och innehållet i en HTML-fil för att extrahera information. Programmerare gör det för att komma åt data, manipulera innehåll eller skrapa webbplatser.

## Hur man gör: 

Bash är inte förstahandsvalet för att parsa HTML, men det kan göras med verktyg som `grep`, `awk`, `sed`, eller externa verktyg som `lynx`. För att vara robusta kommer vi att använda `xmllint` från `libxml2`-paketet.

```bash
# Installera xmllint om det behövs
sudo apt-get install libxml2-utils

# Exempel på HTML
cat > sample.html <<EOF
<html>
<head>
  <title>Exempelsida</title>
</head>
<body>
  <h1>Hej, Bash!</h1>
  <p id="myPara">Bash kan läsa mig.</p>
</body>
</html>
EOF

# Parsa Titeln
title=$(xmllint --html --xpath '//title/text()' sample.html 2>/dev/null)
echo "Titeln är: $title"

# Extrahera stycke efter ID
para=$(xmllint --html --xpath '//*[@id="myPara"]/text()' sample.html 2>/dev/null)
echo "Styckets innehåll är: $para"
```

Utdata:
```
Titeln är: Exempelsida
Styckets innehåll är: Bash kan läsa mig.
```

## Fördjupning

Förr i tiden använde programmerare regex-baserade verktyg som `grep` för att skanna HTML, men det var klumpigt. HTML är inte reguljärt – det är kontextuellt. Traditionella verktyg missar detta och kan vara felbenägna.

Alternativ? Massor. Python med Beautiful Soup, PHP med DOMDocument, JavaScript med DOM-parsrar – språk med bibliotek designade för att förstå HTML:s struktur.

Att använda `xmllint` i bash-skript är solid för enkla uppgifter. Det förstår XML och därmed även XHTML. Vanlig HTML kan vara oförutsägbar, dock. Den följer inte alltid XML:s strikta regler. `xmllint` tvingar HTML in i en XML-modell vilket fungerar bra för välformulerad HTML men kan snubbla på röriga saker.

## Se också

- [W3Schools - HTML DOM Parsare](https://www.w3schools.com/xml/dom_intro.asp): Avmystifierar HTML DOM.
- [MDN Web Docs - Att parsa och serialisera XML](https://developer.mozilla.org/en-US/docs/Web/Guide/Parsing_and_serializing_XML): För XML-parsningsprinciper som gäller XHTML.
- [Beautiful Soup Dokumentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/): Ett Python-bibliotek för HTML-parsning.
- [libxml2 Dokumentation](http://xmlsoft.org/): Detaljer om `xmllint` och relaterade XML-verktyg.
