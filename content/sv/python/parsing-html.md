---
title:                "Åtskilja html"
html_title:           "Python: Åtskilja html"
simple_title:         "Åtskilja html"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/parsing-html.md"
---

{{< edit_this_page >}}

# Vad & Varför?

HTML-parsning är en vanlig uppgift för Python-programmerare. Det innebär att man tar en HTML-fil och extraherar specifika data och attribut från koden. Detta är användbart för att automatisera uppgifter såsom att söka efter specifika länkar eller extrahera information från webbsidor.

# Hur man gör:

### Exempel 1 - Extrahera länkar från en webbsida:

```Python
from bs4 import BeautifulSoup
import requests

# Hämta HTML-koden från en webbsida
page = requests.get("https://www.example.com")

# Skapa en BeautifulSoup-objekt för att parsa HTML-koden
soup = BeautifulSoup(page.content, 'html.parser')

# Hitta alla länkar på sidan och skriv ut dem
links = soup.find_all('a')
for link in links:
    print(link.get('href'))
```

Output:
```
https://www.example.com/link1
https://www.example.com/link2
https://www.example.com/link3
```

### Exempel 2 - Extrahera titlar från ett Google-sökresultat:

```Python
from bs4 import BeautifulSoup
import requests

# Hämta HTML-koden från ett Google-sökresultat
page = requests.get("https://www.google.com/search?q=python")

# Skapa en BeautifulSoup-objekt för att parsa HTML-koden
soup = BeautifulSoup(page.content, 'html.parser')

# Hitta alla titlar på sidan och skriv ut dem
titles = soup.find_all('h3')
for title in titles:
    print(title.text)
```

Output:
```
Python - Din guide till programmeringsspråket
Vad är Python? - Python.org
Python (programspråk) – Wikipedia
```

# Djupdykning:

## Historisk kontext:

Python har länge varit ett populärt programmeringsspråk för automatisering av webbuppgifter. Bibliotek som Beautiful Soup och requests har förenklat webbskrapning och HTML-parsning sedan början av 2000-talet.

## Alternativ:

Det finns flera andra bibliotek för HTML-parsning i Python såsom lxml och Scrapy. Det är viktigt att välja det som passar bäst för ens specifika projekt och dess behov.

## Implementeringsdetaljer:

Vid användning av BeautifulSoup är det viktigt att välja en korrekt parser, beroende på vilken version av Python man använder och vilken typ av HTML-kod man har att hantera. Det är också viktigt att hålla koll på eventuella ändringar i HTML-strukturen på de webbsidor man parsar för att undvika felaktig dataextraktion.

# Se även:

- [Beautiful Soup Dokumentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [requests Dokumentation](https://requests.readthedocs.io/en/master/)
- [Scrapy Dokumentation](https://docs.scrapy.org/en/latest/)