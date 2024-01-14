---
title:                "Python: Das Versenden einer http-Anfrage"
simple_title:         "Das Versenden einer http-Anfrage"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

Das Senden von HTTP-Anfragen ist ein wichtiger Bestandteil der modernen Webentwicklung. Es ermöglicht den Austausch von Informationen zwischen einem Client und einem Server, was in der heutigen Zeit unerlässlich ist. Mit Python können HTTP-Anfragen einfach und effizient ausgeführt werden, was es zu einem Muss für jeden Webentwickler macht.

## Wie geht das?

Um eine HTTP-Anfrage mit Python zu senden, müssen Sie zunächst das eingebaute Modul "urllib" importieren. Dann können Sie die Funktion "urlopen" verwenden, um eine URL zu öffnen und die Antwort der Anfrage zu erhalten.

```
import urllib

response = urllib.request.urlopen('https://www.meinewebseite.de')
print(response.read())
```

Dieses Beispiel sendet eine HTTP-GET-Anfrage an die angegebene URL und gibt den HTML-Inhalt der Webseite als Output aus.

Um eine POST-Anfrage zu senden, können Sie das Modul "urllib.parse" verwenden, um die Parameter als Daten zu codieren und sie mit der Funktion "urlencode" zu verschlüsseln. Dann können Sie diese Daten zusammen mit der URL an die Funktion "urlopen" übergeben, um die Anfrage zu senden.

```
import urllib
import urllib.parse

url = 'https://www.meinewebseite.de/login'
data = urllib.parse.urlencode({'username': 'MaxMustermann', 'password': 'geheimespasswort'}).encode('utf-8')
response = urllib.request.urlopen(url, data)
print(response.read())
```

Dieses Beispiel sendet eine HTTP-POST-Anfrage an die URL für den Anmeldevorgang mit den angegebenen Benutzername und Passwort Daten.

## Tiefere Einblicke

Es gibt noch viele weitere Möglichkeiten, HTTP-Anfragen mit Python zu senden. Zum Beispiel können Sie die Funktion "Request" des Moduls "urllib.request" verwenden, um detailliertere Anfragen zu erstellen, wie z.B. die Angabe von Benutzeragenten oder das Hinzufügen von Cookies. Sie können auch das Modul "requests" verwenden, das zusätzliche Funktionen für die Arbeit mit HTTP-Anfragen bietet.

Es ist auch wichtig zu beachten, dass HTTP-Anfragen nicht immer erfolgreich sein können. In solchen Fällen sollten Sie eine Überprüfung der Statuscodes in der Antwort durchführen, um festzustellen, ob die Anfrage erfolgreich war oder ob ein Fehler aufgetreten ist.

## Siehe auch

- [Dokumentation zu urllib](https://docs.python.org/3/library/urllib.html)
- [Dokumentation zu requests](https://requests.readthedocs.io/)
- [Ein kurzes Tutorial zur Verwendung von Python für HTTP-Anfragen](https://realpython.com/python-requests/)