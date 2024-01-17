---
title:                "HTML-Parsing"
html_title:           "Python: HTML-Parsing"
simple_title:         "HTML-Parsing"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?

Parsing HTML bezieht sich auf die Verarbeitung von HTML-Code, um die darin enthaltenen Informationen zu extrahieren. Programmierer tun dies, um Daten aus Webseiten zu extrahieren und sie für verschiedene Zwecke wie Web Scraping, Datenanalyse oder Webautomatisierung zu nutzen.

## Wie geht's?

```python
# Beispiel-Code zum Parsen von HTML in Python

# Modul BeautifulSoup für das Parsen von HTML importieren
from bs4 import BeautifulSoup

# HTML-Seite herunterladen
url = "https://www.example.com"
r = requests.get(url)
html = r.content

# BeautifulSoup-Objekt erstellen
soup = BeautifulSoup(html, 'html.parser')

# HTML-Tags mit bestimmtem Namen auswählen
title = soup.find("title")
print(title.text)

# Text aus einer bestimmten HTML-Klasse auswählen
paragraph = soup.find("p", class_="sample-class")
print(paragraph.text)

```

## Tief eintauchen

Das Parsen von HTML ist seit den Anfängen des World Wide Web ein wichtiger Bestandteil der Webentwicklung. Früher wurden dafür oft reguläre Ausdrücke verwendet, aber heute gibt es spezialisierte Bibliotheken wie BeautifulSoup oder Scrapy, die das Parsen von HTML viel einfacher machen. Es gibt auch alternative Wege, um an Webseitendaten zu gelangen, wie z.B. Web-APIs, aber das Parsen von HTML bleibt eine nützliche Methode, insbesondere für Seiten ohne API.

## Siehe auch

- [Link zu BeautifulSoup Dokumentation](https://www.crummy.com/software/BeautifulSoup)
- [Link zu Scrapy Dokumentation](https://docs.scrapy.org/en/latest/)
- [Link zu Anleitung für Web Scraping mit BeautifulSoup](https://realpython.com/beautiful-soup-web-scraper-python/)