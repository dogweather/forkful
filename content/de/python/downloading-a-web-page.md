---
title:                "Herunterladen einer Webseite"
html_title:           "Python: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Das Herunterladen einer Webseite kann aus verschiedenen Gründen sinnvoll sein. Vielleicht möchtest du eine Offline-Version einer Webseite zur Verfügung haben, um sie later zu lesen, oder du möchtest eine bestimmte Funktion auf der Webseite automatisiert ausführen.

## Wie geht's

Um eine Webseite in Python herunterzuladen, können wir die `requests` Bibliothek verwenden. Hier ist ein Beispielcode:

```Python
import requests

url = "https://www.beispielwebseite.com"
page = requests.get(url)

# Wir können nun den Inhalt der Seite im Textformat ausgeben
print(page.text)

# Oder den HTML-Code der Seite extrahieren
print(page.content)
```

Die Ausgabe hängt davon ab, was du genau mit den heruntergeladenen Inhalten machen möchtest. Du kannst z.B. bestimmte Teile des HTML-Codes analysieren oder den Text weiterverarbeiten.

## Tiefentauchen

Der `requests` Bibliothek bietet noch viel mehr Funktionen, um mit Webseiten zu interagieren. Du kannst zum Beispiel auch POST-Anfragen senden, um Daten an eine Webseite zu übermitteln. Außerdem hat die Bibliothek integrierte Authentifizierungsfunktionen und kann auch mit SSL-Zertifikaten umgehen.

## Siehe auch

- [Dokumentation der `requests` Bibliothek (auf Englisch)](https://requests.readthedocs.io/en/latest/)
- [Weitere Infos und Beispiele zur Verwendung von `requests` (auf Deutsch)](https://gehrcke.de/2014/02/a-quick-stroll-down-phishing-lane-using-the-requests-library/)