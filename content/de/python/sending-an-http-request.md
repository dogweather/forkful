---
title:                "Eine HTTP-Anfrage senden"
html_title:           "Python: Eine HTTP-Anfrage senden"
simple_title:         "Eine HTTP-Anfrage senden"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Senden von HTTP-Anfragen ist ein wichtiger Aspekt beim Programmieren, da es es ermöglicht, Informationen von einer Website oder einem Server abzurufen. Programmer nutzen es, um Daten zu erhalten und sie in ihre Programme zu integrieren.

## So funktioniert's:
Das Senden von HTTP-Anfragen kann mit Hilfe von Python sehr einfach durchgeführt werden. Hier ist ein Beispiel, wie man eine GET-Anfrage an die Website "www.example.com" senden kann:

```Python
import requests
response = requests.get("http://www.example.com")
print(response.text)
```

Die Ausgabe wird den HTML-Code der Website enthalten. Man kann auch Parameter an die Anfrage anhängen, um bestimmte Informationen zu erhalten:

```Python
import requests
params = {'key': 'value'}
response = requests.get("http://www.example.com", params=params)
print(response.text)
```

Die Ausgabe könnte in diesem Beispiel zum Beispiel eine Liste von Suchergebnissen sein.

## Tiefergehende Informationen:
Das Senden von HTTP-Anfragen hat eine lange Geschichte. Früher wurde es vor allem für einfache HTML-Seiten genutzt, aber heutzutage wird es für die Kommunikation zwischen Servern und die Integration von Web-Services in Programme verwendet.

Es gibt auch alternative Methoden, um Daten von Websites abzurufen, wie z.B. das Scraping von HTML oder die Nutzung von APIs. Diese können je nach Anwendungsbereich möglicherweise bessere Optionen sein.

Um HTTP-Anfragen in Python zu senden, gibt es verschiedene Bibliotheken, die genutzt werden können, wie z.B. Requests, Urllib oder httplib. Jede Bibliothek hat ihre eigenen Vor- und Nachteile, daher ist es wichtig, die Dokumentation zu lesen und die beste Option für das jeweilige Projekt auszuwählen.

## Siehe auch:
- [Requests Library Documentation](https://requests.readthedocs.io/en/master/)
- [Urllib Library Documentation](https://docs.python.org/3/library/urllib.html)
- [httplib Library Documentation](https://docs.python.org/3/library/http.client.html)