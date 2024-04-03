---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:33.722457-07:00
description: "HTML parsen betekent het doorzoeken van de structuur en inhoud van een\
  \ HTML-bestand om informatie te extraheren. Programmeurs doen dit om data te\u2026"
lastmod: '2024-03-13T22:44:50.980237-06:00'
model: gpt-4-0125-preview
summary: HTML parsen betekent het doorzoeken van de structuur en inhoud van een HTML-bestand
  om informatie te extraheren.
title: HTML Parsen
weight: 43
---

## Hoe:
Bash is niet de eerste keuze voor het parsen van HTML, maar het is mogelijk met tools zoals `grep`, `awk`, `sed`, of externe hulpprogramma's als `lynx`. Voor robuustheid gebruiken we `xmllint` uit het `libxml2`-pakket.

```bash
# Installeer xmllint indien nodig
sudo apt-get install libxml2-utils

# Voorbeeld HTML
cat > voorbeeld.html <<EOF
<html>
<head>
  <title>Voorbeeldpagina</title>
</head>
<body>
  <h1>Hallo, Bash!</h1>
  <p id="mijnPara">Bash kan mij lezen.</p>
</body>
</html>
EOF

# Parse de Titel
titel=$(xmllint --html --xpath '//title/text()' voorbeeld.html 2>/dev/null)
echo "De titel is: $titel"

# Paragraaf extraheren op ID
para=$(xmllint --html --xpath '//*[@id="mijnPara"]/text()' voorbeeld.html 2>/dev/null)
echo "De inhoud van de paragraaf is: $para"
```

Output:
```
De titel is: Voorbeeldpagina
De inhoud van de paragraaf is: Bash kan mij lezen.
```

## Diepgaand
Vroeger gebruikten programmeurs regex-gebaseerde tools zoals `grep` om HTML te scannen, maar dat was omslachtig. HTML is niet regulier - het is contextueel. Traditionele tools missen dit en kunnen foutgevoelig zijn.

Alternatieven? Genoeg. Python met Beautiful Soup, PHP met DOMDocument, JavaScript met DOM-parsers—talen met bibliotheken die zijn ontworpen om de structuur van HTML te begrijpen.

Het gebruik van `xmllint` in bash-scripts is solide voor eenvoudige taken. Het begrijpt XML, en daarmee, XHTML. Gewone HTML kan echter onvoorspelbaar zijn. Het volgt niet altijd de strikte regels van XML. `xmllint` dwingt HTML in een XML-model, wat goed werkt voor goed gevormde HTML, maar kan struikelen over rommelige zaken.

## Zie Ook
- [W3Schools - HTML DOM Parser](https://www.w3schools.com/xml/dom_intro.asp): Ontrafelt HTML DOM.
- [MDN Web Docs - Het parsen en serialiseren van XML](https://developer.mozilla.org/nl/docs/Web/Guide/Parsing_and_serializing_XML): Voor XML-parsingprincipes die van toepassing zijn op XHTML.
- [Beautiful Soup Documentatie](https://www.crummy.com/software/BeautifulSoup/bs4/doc/): Een Python-bibliotheek voor het parsen van HTML.
- [libxml2 Documentatie](http://xmlsoft.org/): Details over `xmllint` en gerelateerde XML-tools.
