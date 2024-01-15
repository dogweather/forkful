---
title:                "Eine http Anfrage senden"
html_title:           "Bash: Eine http Anfrage senden"
simple_title:         "Eine http Anfrage senden"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

Wer sich mit dem Thema Webentwicklung oder Automatisierung beschäftigt, wird oft auf den Begriff "HTTP Request" stoßen. Doch was genau verbirgt sich dahinter und warum ist es wichtig, sich damit auseinanderzusetzen? HTTP (Hypertext Transfer Protocol) ist ein grundlegender Bestandteil des World Wide Web und ermöglicht die Kommunikation zwischen Client und Server. Das Versenden von HTTP Requests ist der Schlüssel, um Daten von einem Server zu erhalten oder an diesen zu senden.

## Wie geht's

Um eine HTTP Anfrage zu senden, können verschiedene Tools und Programmiersprachen verwendet werden. In diesem Artikel werden wir uns jedoch auf das Senden von HTTP Requests mit Bash, der Shell eines Linux-Systems, konzentrieren.

Um eine HTTP Anfrage mit Bash zu senden, verwenden wir das Tool `curl`. Es ist bereits in den meisten Linux-Distributionen vorinstalliert und ermöglicht das Senden von HTTP Requests über die Kommandozeile.

Ein einfaches Beispiel für den Aufruf einer Website mit `curl` sieht wie folgt aus:
```Bash
curl www.example.com
```

Dieser Befehl sendet GET Request an die angegebene URL und gibt die HTML des Webservers in der Konsole aus. Möchtest du die Ausgabe stattdessen in eine Datei schreiben, kannst du folgenden Befehl verwenden:
```Bash
curl -o output.html www.example.com
```

Neben GET Requests kann `curl` auch POST Requests senden, die oft zur Übermittlung von Formulardaten verwendet werden. Dazu muss der Content-Type Header angegeben werden und die Daten in das JSON-Format konvertiert werden. Ein Beispiel sieht wie folgt aus:
```Bash
curl -X POST -H "Content-Type: application/json" -d '{"username":"example", "password":"1234"}' www.example.com/login
```

## Tiefergehende Informationen

`curl` bietet viele weitere Funktionen, um HTTP Requests zu konfigurieren und zu automatisieren. Zum Beispiel kann man mit dem `-H` Parameter beliebige Header hinzufügen, mit `-u` einen Benutzernamen und ein Passwort angeben und mit `-s` die Ausgabe der Anfrage komplett deaktivieren.

Eine detaillierte Beschreibung aller Parameter und Funktionen findest du in der offiziellen Dokumentation von `curl`. Auch die `man`-Seite von Bash enthält weitere Informationen und Beispiele.

## Siehe auch

- [Offizielle Dokumentation von `curl`](https://curl.se/docs/)
- [`man`-Seite von Bash](https://linux.die.net/man/1/bash)
- [HTTP Requests mit Python in 60 Sekunden](https://realpython.com/python-requests/)