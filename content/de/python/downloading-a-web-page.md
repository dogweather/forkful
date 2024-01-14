---
title:                "Python: Webseite herunterladen"
simple_title:         "Webseite herunterladen"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Das Herunterladen einer Webseite kann aus verschiedenen Gründen nützlich sein. Man kann beispielsweise Daten aus einer Webseite extrahieren, um sie in einer Datenbank zu speichern oder um sie für Analysen zu verwenden.

## Wie

Das Herunterladen einer Webseite ist in Python sehr einfach. Zunächst muss das `urllib` Modul importiert werden, um die benötigten Funktionen zur Verfügung zu stellen. Dann kann der folgende Code verwendet werden:

```Python
import urllib

url = "https://www.mywebsite.com" # Die URL der Webseite, die heruntergeladen werden soll

response = urllib.request.urlopen(url) # Öffnet die URL und speichert die Antwort in einer Variablen

html = response.read() # Liest den HTML-Code der Webseite

print(html) # Gibt den HTML-Code auf der Konsole aus
```

Als Ergebnis werden Sie den HTML-Code der Webseite auf der Konsole sehen.

## Deep Dive

Um noch tiefer in das Herunterladen einer Webseite einzutauchen, können Sie verschiedene Python-Bibliotheken nutzen, die speziell für diese Zwecke entwickelt wurden. Zum Beispiel bietet das `requests` Modul eine einfachere und benutzerfreundlichere Möglichkeit, auf Webinhalte zuzugreifen. Sie können auch das `BeautifulSoup` Modul verwenden, um den heruntergeladenen HTML-Code zu analysieren und bestimmte Elemente daraus zu extrahieren.

Es ist auch wichtig zu beachten, dass beim Herunterladen einer Webseite nicht nur der HTML-Code heruntergeladen wird, sondern auch alle zugehörigen Ressourcen wie Bilder, CSS-Dateien und JavaScript-Dateien. Sie müssen daher möglicherweise weitere Schritte unternehmen, um alle diese Daten zu erhalten.

## Siehe auch

- [urllib Dokumentation](https://docs.python.org/3/library/urllib.html)
- [requests Dokumentation](https://requests.readthedocs.io/en/master/)
- [BeautifulSoup Dokumentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)