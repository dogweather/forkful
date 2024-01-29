---
title:                "Een webpagina downloaden"
date:                  2024-01-28T21:59:21.463047-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een webpagina downloaden"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/python/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een webpagina downloaden betekent in wezen het grijpen van de gegevens van de URL die je opgeeft en deze op je lokale machine trekken. Programmeurs doen dit om gegevens te parsen, veranderingen te monitoren of interacties met websites te automatiseren.

## Hoe te:

We gebruiken de `requests` bibliotheek van Python. Als je deze niet hebt, installeer het dan met `pip install requests`. Hier is een snel voorbeeld:

```python
import requests

url = 'https://www.example.com'
response = requests.get(url)

if response.ok:
    html_content = response.text
    print(html_content)
else:
    print("Mislukt om de webpagina op te halen")

```

Wanneer dit script draait, als het succesvol is, zie je de HTML-inhoud van "https://www.example.com" uitgeprint in je console.

## Diepgaand

Vóór `requests`, had Python `urllib`. Het bestaat nog steeds, maar `requests` heeft de show gestolen met zijn eenvoudigere, gebruiksvriendelijke interface. `requests` werd uitgebracht in 2011 door Kenneth Reitz en is sindsdien de gouden standaard voor HTTP in Python. Maar het gaat niet alleen om eenvoud - `requests` is ook robuust en biedt functies zoals sessie-objecten, cookie-persistentie en automatische afhandeling van SSL-certificaten.

Er zijn alternatieven zoals `http.client`, dat lager niveau is dan `requests`, en externe bibliotheken zoals `aiohttp` voor asynchrone bewerkingen. Diep onder de motorkap, ongeacht je keuze, gaan deze bibliotheken om met web servers, versturen HTTP-verzoeken en behandelen antwoorden.

Bij het downloaden van pagina's is het belangrijk om de regels van de weg te overwegen: respecteer `robots.txt` bestanden om te weten waar je toegestaan bent, en hamer niet op servers - vertraag je verzoeken. Wees ook bedacht op het feit dat webpagina's dynamische inhoud met JavaScript kunnen binnenhalen, wat niet wordt vastgelegd met een eenvoudig HTTP-verzoek.

## Zie Ook:

- `requests` documentatie: https://requests.readthedocs.io/en/master/
- `urllib` info: https://docs.python.org/3/library/urllib.html
- `robots.txt` intro: https://www.robotstxt.org
- `aiohttp` voor asynchrone webverzoeken: https://docs.aiohttp.org/en/stable/
