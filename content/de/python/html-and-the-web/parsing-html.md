---
title:                "HTML parsen"
aliases:
- /de/python/parsing-html/
date:                  2024-02-03T19:12:39.581886-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML parsen"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen von HTML beinhaltet die Analyse des HTML-Codes einer Webseite, um spezifische Informationen oder Elemente zu extrahieren. Es ist eine gängige Aufgabe beim Web Scraping, Data Mining oder Automatisieren von Interaktionen mit Websites. Programmierer tun dies, um programmatisch mit Websites zu interagieren oder Daten von ihnen zu extrahieren, Aufgaben zu automatisieren oder Webanwendungen zu testen.

## Wie geht das:
Python bietet leistungsstarke Bibliotheken wie BeautifulSoup und requests für Web Scraping und HTML-Parsing. Um zu beginnen, müssen Sie diese Bibliotheken installieren, falls Sie dies noch nicht getan haben:

```bash
pip install beautifulsoup4 requests
```

Hier ist ein einfaches Beispiel, das `requests` verwendet, um den HTML-Inhalt einer Webseite abzurufen, und `BeautifulSoup`, um ihn zu parsen:

```python
import requests
from bs4 import BeautifulSoup

# Den Inhalt einer Webseite abrufen
URL = 'https://example.com'
page = requests.get(URL)

# Den HTML-Inhalt parsen
soup = BeautifulSoup(page.content, 'html.parser')

# Beispiel für das Extrahieren des Titels der Webseite
title = soup.find('title').text
print(f'Webseitentitel: {title}')
```

**Beispielausgabe**:
```
Webseitentitel: Beispiel-Domain
```

Für komplexere Anfragen, wie das Extrahieren aller Links von einer Webseite, können Sie die verschiedenen Methoden von BeautifulSoup zum Navigieren und Durchsuchen des Parse-Baums verwenden:

```python
# Alle Links innerhalb von <a>-Tags extrahieren
links = soup.find_all('a')

for link in links:
    href = link.get('href')
    print(href)
```

**Beispielausgabe**:
```
https://www.iana.org/domains/example
```

Die Flexibilität von BeautifulSoup erlaubt es Ihnen, Ihre Suche nach den exakt benötigten Daten anzupassen, was das HTML-Parsing zu einem mächtigen Werkzeug für Programmierer macht, die mit Webinhalten arbeiten.
