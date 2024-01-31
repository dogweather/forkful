---
title:                "HTML parsen"
date:                  2024-01-20T15:33:11.216226-07:00
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"

category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTML-Parsing ist das Umwandeln von HTML-Dokumenten in eine Struktur, die von Software verarbeitet werden kann. Programmierer machen das, um Inhalte von Webseiten automatisch zu extrahieren und zu manipulieren.

## How to:
Zum Parsen von HTML in Python verwenden wir BeautifulSoup. Es ist einfach und effektiv. Hier ein Beispiel:

```Python
from bs4 import BeautifulSoup
import requests

url = 'https://example.com'
response = requests.get(url)
html_content = response.text

soup = BeautifulSoup(html_content, 'html.parser')
title = soup.find('title').get_text()
print(title)
```

Wenn `https://example.com` einen Titel-Tag mit "Beispiel-Seite" hat, wird die Ausgabe:

```
Beispiel-Seite
```

## Deep Dive
Historisch gesehen, wurde HTML-Parsing mit regulären Ausdrücken oder komplexen String-Operationen gemacht. Aber das ist fehleranfällig und ineffizient. BeautifulSoup hingegen nutzt einen Parser, der die Struktur von HTML versteht. Dies vereinfacht das Extrahieren von Informationen erheblich.

Als Alternative zu BeautifulSoup gibt es auch noch `lxml` oder `html.parser` in Python, aber BeautifulSoup bietet eine benutzerfreundlichere API. 

Implementierungsdetails: BeautifulSoup wandelt HTML in ein Baumdiagramm von Objekten um. Dadurch wird es einfacher, Elemente zu finden und zu bearbeiten. Man kann nach Tags, Klassen, IDs und mehr suchen.

## See Also
- BeautifulSoup Dokumentation: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- `requests` Bibliothek: https://docs.python-requests.org/
- Einführung zu HTML und CSS: https://developer.mozilla.org/de/docs/Learn/Getting_started_with_the_web/HTML_basics
