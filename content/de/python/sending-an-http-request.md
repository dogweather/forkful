---
title:                "Das Senden einer http-Anfrage"
html_title:           "Python: Das Senden einer http-Anfrage"
simple_title:         "Das Senden einer http-Anfrage"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

HTTP-Anfragen sind ein wesentlicher Bestandteil der modernen Webentwicklung. Sie ermöglichen es, Daten zwischen Servern und Clients auszutauschen und sind somit unerlässlich für die Interaktion mit Webanwendungen. In diesem Artikel erfahren Sie, wie Sie mit Python HTTP-Anfragen senden und empfangen können.

## Wie man es macht

Um eine HTTP-Anfrage mit Python zu senden, müssen Sie zunächst das `requests`-Modul installieren. Dies kann einfach mit dem Befehl `pip install requests` in Ihrer Befehlszeile durchgeführt werden.

Sobald das Modul installiert ist, können Sie es in Ihrem Code importieren:

```Python
import requests
```

Als nächstes müssen Sie eine URL festlegen, an die Sie die Anfrage senden möchten, und die gewünschten Parameter und Daten angeben. Zum Beispiel, um eine GET-Anfrage an die Website "www.example.com" zu senden, könnten Sie Folgendes tun:

```Python
url = "https://www.example.com"
params = {"key": "value"}
```

Jetzt können Sie `requests` verwenden, um Ihre Anfrage zu senden:

```Python
response = requests.get(url, params=params)
```

Die Antwort wird als `response`-Objekt zurückgegeben, das verschiedene Informationen wie den Statuscode und die empfangene Daten enthält. Um den Inhalt der Antwort zu erhalten, können Sie die `text`-Methode verwenden:

```Python
print(response.text)
```

In diesem Beispiel würden Sie den HTML-Code der Website "www.example.com" ausgeben.

## Tiefer in die Materie eintauchen

Neben GET-Anfragen, die für das Abrufen von Daten verwendet werden, können Sie mit `requests` auch POST-Anfragen senden. POST-Anfragen werden normalerweise verwendet, um Daten an einen Server zu senden, z.B. beim Ausfüllen eines Formulars. Um eine POST-Anfrage zu senden, müssen Sie zusätzlich die zu sendenden Daten angeben:

```Python
url = "https://www.example.com/login"
data = {"username": "user1", "password": "123456"}
response = requests.post(url, data=data)
```

Sie können auch einen Header mit zusätzlichen Informationen zu Ihrer Anfrage hinzufügen, indem Sie ein `headers`-Argument verwenden:

```Python
headers = {"Content-Type": "application/json"}
response = requests.post(url, data=data, headers=headers)
```

Es gibt viele weitere Optionen und Funktionen, die Sie beim Senden von HTTP-Anfragen mit Python verwenden können. Für weitere Details und Beispiele empfehle ich Ihnen, die offizielle Dokumentation von `requests` zu lesen.

## Siehe auch

- [Offizielle Dokumentation von Requests](https://requests.readthedocs.io/en/master/)
- [Python - Requests Modul](https://www.geeksforgeeks.org/python-requests-module/)
- [Making HTTP Requests in Python](https://stackabuse.com/making-http-requests-in-python-using-pythons-requests-module/)