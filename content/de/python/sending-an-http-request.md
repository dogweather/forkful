---
title:                "Eine HTTP-Anforderung senden"
html_title:           "Bash: Eine HTTP-Anforderung senden"
simple_title:         "Eine HTTP-Anforderung senden"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Python HTTP-Anfragen verstehen und erstellen
## Was & Warum?
HTTP-Anfragen sind eine Methode, mit der ein Programm oder eine Anwendung Informationen von einem Server abruft. Programmierer verwenden dies, um Daten aus APIs zu extrahieren, Inhalte dynamisch zu laden oder Strukturdaten für Scraping-Aufgaben zu holen.

## Wie geht das:
Es gibt viele Methoden, um eine HTTP-Anfrage in Python zu senden. Eine der beliebtesten ist die Bibliothek `requests`. Hier ist, wie man es macht:

```Python
import requests

response = requests.get('http://www.example.com')
print(response.text)
```

Ausgabe:

```Python
<!doctype html>
<html>
<head>
    <title>Beispiel Website</title>
</head>
<body>
    <h1>Beispiel Website</h1>
    <p>Hallo, Willkommen auf www.example.com!</p>
</body>
</html>
```
## Deep Dive
Historisch gesehen war das Senden von HTTP-Anfragen keine einfache Aufgabe in Python. Bevor `requests` eingeführt wurde, mussten Entwickler komplizierte Bibliotheken wie `urllib2` verwenden. Heute ist `requests` die bevorzugte Methode, da sie einfach zu verwenden und gut dokumentiert ist.

Alternativen zu `requests` umfassen `http.client` (Standardbibliothek), `httplib2`, `treq` und `pycurl`. Diese bieten unterschiedliche Verwendungsmuster und Eigenschaften an, können aber überwältigend sein und sind oft übermütig für grundlegende Aufgaben.

Auf unterster Ebene öffnet eine HTTP-Anfrage einfach eine Netzwerkverbindung zu einem Webserver und sendet eine standardisierte Nachricht, die eine "GET"- oder "POST"-Operation anfordert. Der Server entschlüsselt diese Nachricht und sendet eine Antwort, die der Client dann interpretiert.

## Siehe auch
Für weitere Informationen und Tutorials über HTTP-Anfragen in Python, besuchen Sie bitte die folgenden Seiten:

1. Dokumentation der `requests` Bibliothek: https://docs.python-requests.org/en/latest/
2. HTTP-Anfragen in Python, Einführung: https://realpython.com/python-requests/
3. Schnelle Anfängereinführung von Mozilla: https://developer.mozilla.org/de/docs/Learn/Python/Quickly_Learn_Object_Oriented_Programming