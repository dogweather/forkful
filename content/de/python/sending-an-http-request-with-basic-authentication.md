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

## Warum
Das Senden von HTTP-Anfragen mit Basic Authentication ist eine Möglichkeit, sicher auf geschützte Ressourcen zuzugreifen. Dies kann beispielsweise für die Integration von externen APIs oder das Zugreifen auf Bereiche einer Website erforderlich sein, die nur für autorisierte Benutzer zugänglich sind.

## Wie geht's
Um eine HTTP-Anfrage mit Basic Authentication zu senden, benötigen wir die `requests` Bibliothek in Python. Als Erstes müssen wir die notwendigen Daten, wie Benutzername und Passwort, in einer `auth` Variable speichern. Dann können wir die Anfrage mit der `get()` oder `post()` Methode senden und die `auth` Variable angeben.

```Python
import requests

url = "https://meine-website.com/geschuetzte-bereiche"
auth = ("benutzername", "passwort")

# Eine GET-Anfrage senden
response = requests.get(url, auth=auth)

# Eine POST-Anfrage senden
data = {"name": "Max", "alter": 30}
response = requests.post(url, auth=auth, data=data) 

print(response.text)  # Ausgabe der Antwort
```

Die `response` Variable enthält die Antwort auf unsere Anfrage. Wir können auf verschiedene Daten wie den Statuscode, die Header oder den Inhalt der Antwort zugreifen.

## Tiefer eintauchen
Der Basic Authentication-Mechanismus verwendet das Authentifizierungsschema `Basic`, das durch die Übermittlung von Benutzername und Passwort im Klartext gekennzeichnet ist. Um die Sicherheit zu erhöhen, wird empfohlen, HTTPS zu verwenden, um die Daten während der Übertragung zu verschlüsseln. Alternativ kann auch das sicherere Authentifizierungsschema `Digest` verwendet werden, bei dem das Passwort nicht im Klartext übertragen wird.

## Siehe auch
- [Python requests Dokumentation](https://requests.readthedocs.io/en/master/)
- [HTTP Basic Authentication](https://developer.mozilla.org/de/docs/Web/HTTP/Basic_access_authentication)