---
title:                "Herunterladen einer Website"
html_title:           "Python: Herunterladen einer Website"
simple_title:         "Herunterladen einer Website"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Herunterladen einer Webseite ist der Prozess, bei dem man den HTML-Code und die Inhalte einer Webseite auf seinen Computer lädt. Programmierer nutzen dieses Verfahren, um den Code einer Webseite zu analysieren, Inhalte zu extrahieren oder eine lokale Kopie der Webseite zu speichern.

# Wie geht das?
Der folgende Python-Code zeigt, wie man mit dem Modul "requests" eine Webseite herunterladen und den HTML-Code ausgeben kann.

```Python
import requests

url = "https://www.python.org/"
response = requests.get(url)
print(response.text)
```

Der obige Code verwendet die Funktion "get" aus dem "requests" Modul, um eine HTTP-Anfrage an die angegebene URL zu senden. Der HTML-Code wird dann in der Variable "response" gespeichert und mit der Funktion "text" ausgegeben. So einfach ist es!

## Deep Dive
Das Herunterladen von Webseiten hat in den letzten Jahren enorm an Bedeutung gewonnen, da immer mehr Inhalte im Internet verfügbar sind. In der Vergangenheit war es üblich, eine Webseite "manuell" über den Browser herunterzuladen, was jedoch zeitaufwändig und umständlich sein kann. Durch die Verwendung von Programmierwerkzeugen wie Python können Webseiten viel effizienter analysiert und verarbeitet werden.

Es gibt auch andere Alternativen für das Herunterladen von Webseiten, wie beispielsweise das "urllib" Modul in Python, das ähnliche Funktionen bietet. Ein weiterer Ansatz ist die Verwendung von sogenannten "Web Scraping" Tools, die speziell für das Extrahieren von Daten aus Webseiten entwickelt wurden.

Die Implementierung des Herunterladens einer Webseite in Python ist relativ einfach und erfordert nur wenige Zeilen Code. Das "requests" Modul bietet auch eine Vielzahl von Funktionen und Optionen für komplexere Anfragen und die Verarbeitung von Daten.

## Siehe auch
Hier sind einige zusätzliche Ressourcen zum Thema "web page downloading":
- [Offizielle Python Dokumentation zu "requests"](https://docs.python-requests.org/en/latest/)
- [Ein Tutorial zum Herunterladen von Webseiten mit Python](https://realpython.com/python-requests/)
- [Weitere Informationen zu "Web Scraping" Tools](https://www.dataquest.io/blog/web-scraping-tutorial-python/)