---
date: 2024-01-20 15:33:14.375537-07:00
description: 'How to: (Hur?) Utmatning.'
lastmod: '2024-04-05T21:53:38.807601-06:00'
model: unknown
summary: ''
title: Tolka HTML
weight: 43
---

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
