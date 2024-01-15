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

## Warum

HTML ist die gängige Sprache zur Formatierung von Webseiten, aber manchmal möchte man die darin enthaltenen Informationen extrahieren und weiterverarbeiten. Hier kommt das Parsen von HTML ins Spiel, um den Text und die Daten aus den Tags zu extrahieren und sie in einem verarbeitbaren Format zu erhalten.

## Wie

Zur Demonstration verwenden wir die Python-Bibliothek BeautifulSoup, die das Parsen von HTML stark vereinfacht. Zunächst müssen wir BeautifulSoup installieren:

```Python
pip install beautifulsoup4
```

Als nächstes importieren wir die Bibliothek und geben unseren HTML-Code als Parameter an:

```Python
from bs4 import BeautifulSoup

html = "<html><head><title>Beispiel</title></head><body><h1>Willkommen</h1><p>Das ist ein Beispieltext.</p></body></html>"
```

Im nächsten Schritt erstellen wir ein BeautifulSoup-Objekt und übergeben ihm den HTML-Code und den Parser, den wir verwenden möchten (hier verwenden wir den Standardparser "html.parser"):

```Python
soup = BeautifulSoup(html, 'html.parser')
```

Jetzt können wir mithilfe von BeautifulSoup die gewünschten Informationen aus dem HTML-Code extrahieren. Zum Beispiel können wir den Titel der Seite abrufen:

```Python
title = soup.title
print(title)
# Ausgabe: <title>Beispiel</title>
```

Oder wir können die Überschriften der Seite in einer Liste ausgeben:

```Python
headings = soup.find_all('h1')
for heading in headings:
    print(heading.text)
# Ausgabe: Willkommen
```

## Deep Dive

Es gibt viele Methoden und Techniken, die beim Parsen von HTML verwendet werden können, und BeautifulSoup bietet eine umfangreiche Dokumentation, die dabei helfen kann, die gewünschten Informationen aus dem Code zu extrahieren. Hier sind einige nützliche Methoden, die in verschiedenen Situationen verwendet werden können:

- `find()`: Gibt das erste Vorkommen eines Tags zurück
- `find_all()`: Gibt eine Liste aller Vorkommen eines Tags zurück
- `get()`: Gibt den Wert eines bestimmten Attributs zurück
- `attrs`: Gibt alle Attribute eines Tags zurück
- `text`: Gibt den Text innerhalb eines Tags zurück

Es ist auch möglich, mithilfe von CSS-Selektoren bestimmte Tags auszuwählen:

```Python
soup.select('h1') # wählt alle h1-Überschriften aus
soup.select('p#example') # wählt das p-Tag mit dem id-Attribut "example" aus
```

Es gibt noch viele weitere Möglichkeiten, mit BeautifulSoup HTML zu parsen, und es lohnt sich, sich mit der Dokumentation auseinanderzusetzen, um alle Funktionen und Möglichkeiten zu entdecken.

## Siehe auch

- BeautifulSoup Dokumentation: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- Ein Tutorial zum Parsen von HTML mit BeautifulSoup: https://www.freecodecamp.org/news/scraping-multiple-pages-with-beautifulsoup-and-python-2a0fcae2c6f1/ 
- Weitere Informationen und Beispiele: https://realpython.com/beautiful-soup-web-scraper-python/