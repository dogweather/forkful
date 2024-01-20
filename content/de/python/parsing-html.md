---
title:                "HTML parsen"
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/parsing-html.md"
---

{{< edit_this_page >}}

# Was & Warum?

HTML zu parsen heißt, man durchsucht und analysiert ein HTML-Dokument, um es effektiv nutzbar zu machen. Programmierer tun dies, um bestimmte Informationen aus Webseiten zu extrahieren oder um Web-Scraping zu betreiben.

# So geht's:

Um mit Python HTML zu parsen, verwenden wir die Bibliothek BeautifulSoup. Hier ist der grundlegende Ablauf:

```Python
from bs4 import BeautifulSoup
import requests

url = "http://www.irgendeinewebsite.de"
antwort = requests.get(url)
soup = BeautifulSoup(antwort.text, "html.parser")

überschriften = soup.find_all("h1")    # Findet alle Überschriften
for überschrift in überschriften:
    print(überschrift.text.strip())   # Zeigt den Text ohne zusätzlichen Leerraum
```
Bearkeitungsergebnis könnte so aussehen:

```Python
"Erste Überschrift"
"Zweite Überschrift"
```

# Tiefgreifende Informationen:

Geschichtlich gesehen wurde HTML-Parsing entwickelt, um die strukturierten Daten, die in HTML-Dokumenten verborgen sind, lesbar und nutzbar zu machen.

Alternativ zum Parsen von HTML könnten wir beispielsweise screen scraping verwenden. Dennoch ist HTML-Parsing genauer und effizienter, da es auf die HTML-Struktur selbst ausgerichtet ist.

Die Implementierungsdetails von BeautifulSoup basieren auf dem Durchlaufen des HTML-Baums: Jedes Element des Dokuments wird zu einem Python-Objekt, das dann verarbeitet werden kann.

# Siehe auch:

Für weiterführende Informationen über BeautifulSoup und HTML-Parsing in Python, könnten die folgenden Quellen hilfreich sein:

- BeautifulSoup Dokumentation: https://beautiful-soup-4.readthedocs.io/en/latest/
- Python Requests Bibliothek: https://docs.python-requests.org/en/latest/
- Mehr über Web scraping: https://realpython.com/beautiful-soup-web-scraper-python/