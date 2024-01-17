---
title:                "Eine http-Anfrage mit grundlegender Authentifizierung senden"
html_title:           "Python: Eine http-Anfrage mit grundlegender Authentifizierung senden"
simple_title:         "Eine http-Anfrage mit grundlegender Authentifizierung senden"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Senden einer HTTP-Anfrage mit grundlegender Authentifizierung ist eine Möglichkeit für Programmierer, Daten von einem Server abzurufen, der eine Authentifizierung erfordert. Dies ist wichtig, um auf geschützte Daten zuzugreifen und sicherzustellen, dass nur autorisierte Benutzer darauf zugreifen können.

# Wie geht das?
```python
import requests

# URL und Benutzerdaten definieren
url = "https://example.com/api/users"
username = "john"
password = "password"

# Authentifizierung hinzufügen und HTTP-Anfrage senden
r = requests.get(url, auth=(username, password))

# Antwort ausgeben
print(r.json())
```

Das obige Beispiel zeigt, wie man eine HTTP-Anfrage mit grundlegender Authentifizierung in Python sendet. Wir importieren das requests-Modul und definieren die URL und die Benutzerdaten. Dann fügen wir die Authentifizierung zur HTTP-Anfrage hinzu und senden sie. Schließlich geben wir die Antwort als JSON-Format aus.

# Tiefgehende Einblicke
## Historischer Hintergrund
HTTP steht für Hypertext Transfer Protocol und ist das Hauptprotokoll, das im World Wide Web verwendet wird. Ursprünglich wurde es entwickelt, um HTML-Dokumente auszutauschen, aber jetzt wird es für eine Vielzahl von Anfragen und Antworten verwendet, einschließlich dem Abrufen von Daten von einem Server. Die grundlegende Authentifizierung wurde als einfache Methode eingeführt, um Benutzer zu verifizieren, und wird von vielen APIs noch immer verwendet.

## Alternativen
Es gibt verschiedene Arten der Authentifizierung, die verwendet werden können, um eine HTTP-Anfrage zu senden. Die grundlegende Authentifizierung ist die einfachste Methode, aber sie hat auch Nachteile wie die Übertragung von Passwörtern im Klartext. Eine sicherere Methode ist die Verwendung von Token oder OAuth, die jedoch komplexer zu implementieren sind.

## Implementierungsdetails
Um eine grundlegende Authentifizierung durchzuführen, müssen Sie den Header "Authorization" zu Ihrer HTTP-Anfrage hinzufügen. Dieser Header enthält den Benutzernamen und das Passwort in einem Base64-kodierten Format. Der Server, der die Anfrage empfängt, entschlüsselt die Anmeldeinformationen und überprüft sie.

# Siehe auch
- [HTTP authentication types](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Requests library documentation](https://requests.readthedocs.io/en/master/user/authentication/)