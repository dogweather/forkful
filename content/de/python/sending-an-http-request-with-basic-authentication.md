---
title:                "Python: Senden einer http Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer http Anfrage mit grundlegender Authentifizierung"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum

Das Senden von HTTP-Anfragen mit Basissauthentifizierung ist ein wichtiger Prozess während der Programmierung, da es ermöglicht, auf geschützte Ressourcen zuzugreifen. Dies kann hilfreich sein, wenn man beispielsweise mit APIs arbeitet oder auf bestimmte Online-Dienste zugreifen möchte. Es ist eine grundlegende Technik, die von vielen Programmierern täglich verwendet wird.

## So geht's

Die HTTP-Anfragen mit Basissauthentifizierung können in Python über das Modul "requests" realisiert werden. Zunächst müssen wir das Modul importieren:

```Python 
import requests 
```

Als nächstes müssen wir die URL der Ressource angeben, auf die wir zugreifen möchten:

```Python 
url = "https://mywebsite.com/api"
```

Dann müssen wir einen Benutzernamen und ein Passwort für die Authentifizierung angeben:

```Python 
auth = ("username", "password")
```

Schließlich können wir die Anfrage senden und die Antwort erhalten:

```Python 
response = requests.get(url, auth=auth)
print(response.text)
```

Das Ergebnis wird in der Konsole ausgegeben und sollte den Zugriff auf die geschützte Ressource bestätigen. 

## Tiefergehende Information

Bei der Verwendung der Basissauthentifizierung wird der Benutzername und das Passwort im Klartext übertragen, was ein potentielles Sicherheitsrisiko darstellt. Daher ist es wichtig, dass die Kommunikation über HTTPS erfolgt, um die Daten zu verschlüsseln. Außerdem sollte das Passwort regelmäßig geändert werden, um die Sicherheit zu erhöhen.

Ein weiterer wichtiger Aspekt ist, dass die Benutzerauthentifizierung über die HTTP-Anfrage erfolgt und nicht über das HTTP-Header. Dies bedeutet, dass das Passwort bei jeder Anfrage im Klartext übertragen wird, was ebenfalls ein Sicherheitsrisiko darstellen kann.

## Siehe auch

- Offizielle Dokumentation zu "requests": https://requests.readthedocs.io/en/master/
- Ein Beispielprojekt für die Verwendung von "requests" für eine HTTP-Anfrage mit Basissauthentifizierung: https://github.com/exampleproject
- Ein Artikel über die Sicherheit bei der Verwendung von Basissauthentifizierung: https://blog.example.com/security-considerations-basic-auth/