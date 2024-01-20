---
title:                "Tolka HTML"
date:                  2024-01-20T15:33:14.375537-07:00
html_title:           "Arduino: Tolka HTML"
simple_title:         "Tolka HTML"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Vad & Varför?)
Parsing HTML innebär att omvandla HTML-kod till en struktur som ett program eller skript kan förstå och manipulera. Programmerare gör detta för att interagera med webbsidor, extrahera data, automatisera webbuppgifter eller testa webbapplikationer.

## How to: (Hur?)
```python
from bs4 import BeautifulSoup
import requests

# Hämta HTML från en webbsida
response = requests.get('https://example.com')
html_doc = response.text

# Parse HTML
soup = BeautifulSoup(html_doc, 'html.parser')

# Extrahera data
headline = soup.find('h1').text

print(headline)
```
Utmatning:
```
Exempel Domän
```

## Deep Dive (Djupdykning)
Parsing HTML är ett hörnsten i webbskrapning och automatiska tester sedan HTML blev webbens standard. Tidiga verktyg inkluderade programbibliotek som `HTMLParser` i Python, medans moderna alternativ som `BeautifulSoup` och `lxml` erbjuder mer kraftfulla och användarvänliga funktioner. Dessa bibliotek hanterar illa formaterad HTML och kan navigera i DOM-trädet (Document Object Model) smidigt. BeautifulSoup bygger på 'html.parser' (standard Python-parsern) eller 'lxml', beroende på användarens behov av hastighet kontra flexibilitet.

## See Also (Se Också)
- Beautiful Soup documentation: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- `lxml` library: https://lxml.de/
- Python requests library: https://requests.readthedocs.io/en/master/
- The HTML standard: https://html.spec.whatwg.org/