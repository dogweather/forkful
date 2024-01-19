---
title:                "Analysera html"
html_title:           "Arduino: Analysera html"
simple_title:         "Analysera html"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att parse HTML innebär att man går igenom HTML-kod för att extrahera specifik information. Programmerare gör detta för att samla data från webbsidor - allt från att skrapa webbplatser till att interagera med webbtjänster.

## Så här gör du:

Python har flera bibliotek för att parse HTML, men vi kommer att använda 'BeautifulSoup' för att visa hur man gör det. Låt oss anta att vi vill hämta alla länkar från en webbsida:

```Python
from bs4 import BeautifulSoup
import requests

response = requests.get('https://www.example.com')

soup = BeautifulSoup(response.text, 'html.parser')

for link in soup.find_all('a'):
    print(link.get('href'))
```

Exekveringen av koden ovan kommer att ge oss alla URL: er som finns på hemsidan 'https://www.example.com'.

## Djupdykning

HTML parsing har sina rötter i de tidiga dagarna av webbutveckling, när data mellan webbplatser delades genom HTML snarare än API:er. Det finns också alternativ till 'BeautifulSoup', som 'lxml' och 'html.parser'.

Implementationen varierar beroende på typen av parsare. Simpla parsare (t.ex. 'html.parser') använder reguljära uttryck för att identifiera HTML taggar, medan mer sofistikerade alternativ (t.ex. 'lxml') kan använda metoder som DOM traversal.

## Se även

1. Officiell dokumentation för BeautifulSoup: https://www.crummy.com/software/BeautifulSoup/bs4/doc/

2. Python 'requests' bibliotek: https://docs.python-requests.org/en/latest/

3. Tutorial om webbskrapning med Python: https://realpython.com/beautiful-soup-web-scraper-python/