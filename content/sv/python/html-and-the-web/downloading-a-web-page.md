---
date: 2024-01-20 17:44:45.834180-07:00
description: "How to: Vi anv\xE4nder `requests` f\xF6r att ladda ner en webbsida enkelt.\
  \ S\xE5 h\xE4r."
lastmod: '2024-03-13T22:44:37.481810-06:00'
model: gpt-4-1106-preview
summary: "Vi anv\xE4nder `requests` f\xF6r att ladda ner en webbsida enkelt."
title: "H\xE4mta en webbsida"
weight: 42
---

## How to:
Vi använder `requests` för att ladda ner en webbsida enkelt. Så här:

```python
import requests

url = 'https://www.exempelsida.se'
response = requests.get(url)

if response.status_code == 200:
    html_content = response.text
    print(html_content[:500])  # Skriver ut de första 500 tecknen
else:
    print("Webbsidan kunde inte laddas ned. Statuskod:", response.status_code)
```

Om allt går bra ser output ungefär ut så här:

```python
<!DOCTYPE html>
<html>
<head>
    <title>Din Exempelsida</title>
    ...
</head>
<body>
    ...
    (HTML-innehåll fortsätter)
</body>
</html>
```

## Deep Dive
Långt innan `requests` fanns, använde vi `urllib`. `requests` är dock mer intuitivt och kraftfullt. Alternativt finns `Scrapy` för större, mer komplexa skrapningsprojekt.

Laddning av en webbsida kan variera i komplexitet. Många moderna sidor laddar innehåll dynamiskt med JavaScript. I sådana fall krävs verktyg som `Selenium` eller `BeautifulSoup` med `requests_html` för att efterlikna en webbläsare.

Att bara "hämta" en sida är inte alltid nog. Etik och juridik som robots.txt och copyright gäller. Använd alltid API:er om de finns tillgängliga och respektera webbsidornas användarvillkor.

## See Also
- [requests dokumentation](https://docs.python-requests.org/en/latest/)
- [Scrapy officiell webbplats](https://scrapy.org/)
- [BeautifulSoup dokumentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [Selenium dokumentation](https://selenium-python.readthedocs.io/)
