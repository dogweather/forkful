---
title:                "Eine Webseite herunterladen"
html_title:           "Arduino: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?
Durch das Herunterladen einer Webseite können wir ihren Inhalt erfassen und analysieren. Programmierer machen das oft, um Daten zu sammeln, die sie dann in ihren Anwendungen verwenden können.

## So geht's:
Mit `requests` Modul in Python ist es einfach:

```Python
import requests

def herunterladen(url):
    response = requests.get(url)
    return response.content

print(herunterladen("https://www.wikipedia.org/"))
```

Dieser Beispielcode holt die Inhalte der Wikipedia-Startseite.

## Deep Dive
Historisch gesehen nutzte man `urllib`, um Webseiten in Python herunterzuladen. Aber `requests` hat sich durchgesetzt wegen seiner Einfachheit und Effizienz.

Eine Alternative dazu wäre `http.client` oder `httplib2`. Aber beide sind umständlicher im Vergleich zu `requests`.

Die `requests.get(url)` Funktion unter der Haube schickt eine GET Anfrage an die gegebene URL und gibt eine Antwort zurück, wo wir mit `response.content` den Inhalt extrahieren können.

## Weiterführende Informationen
- Die offizielle Dokumentation von `requests`: [https://requests.readthedocs.io](https://requests.readthedocs.io)
- Eine gute Einleitung zu `requests`: [https://realpython.com/python-requests/](https://realpython.com/python-requests/)
- `http.client` Dokumentation: [https://docs.python.org/3/library/http.client.html](https://docs.python.org/3/library/http.client.html)
- `httplib2` auf GitHub: [https://github.com/httplib2/httplib2](https://github.com/httplib2/httplib2)