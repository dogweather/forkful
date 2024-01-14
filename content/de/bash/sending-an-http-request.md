---
title:                "Bash: Das Senden einer http-Anfrage"
simple_title:         "Das Senden einer http-Anfrage"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

Ein HTTP Request zu senden ist eine grundlegende Fähigkeit, die jeder Bash Programmierer kennen sollte. Es ermöglicht dir, mit anderen Webseiten und Servern zu interagieren und Daten auszutauschen. Dies ist besonders nützlich, wenn du Skripte schreibst, die automatisierte Tasks ausführen und mit externen APIs kommunizieren müssen.

## Wie man das macht

Das Senden eines HTTP Requests in Bash erfordert die Verwendung des Befehls `curl`. Mit `curl` kannst du einen HTTP Request an eine bestimmte URL senden und sogar Parameter und Header hinzufügen. Schau dir dieses Beispiel an:

```Bash
curl -X GET https://api.example.com/users?id=12345 -H "Authorization: Bearer abcdefg123"
```

In diesem Beispiel wird ein GET Request an die URL `https://api.example.com/users` gesendet und die Parameter `id=12345` werden hinzugefügt. Der Header `Authorization` mit dem Wert `Bearer abcdefg123` wird ebenfalls angegeben.

Die Ausgabe dieses Befehls wird den Inhalt der empfangenen Antwort anzeigen. Du kannst auch die Option `-o` verwenden, um die Antwort in eine Datei zu schreiben, oder `-i` um die Header der Antwort anzuzeigen.

## Tiefere Einblicke

Es gibt noch viele weitere Optionen, die du beim Senden von HTTP Requests mit `curl` verwenden kannst. Zum Beispiel kannst du die Methode des Requests mit `-X` angeben, den zu verwendenden Port mit `-p` festlegen oder eine Benutzername und Passwort für eine HTTP Basic Authentifizierung mit `-u` hinzufügen. Es lohnt sich, ein wenig Zeit zu investieren, um die verschiedenen Möglichkeiten des Befehls `curl` kennenzulernen und auszuprobieren.

## Siehe auch

- Offizielle Dokumentation von `curl`: https://curl.haxx.se/docs/manpage.html
- Ein Tutorial zum Senden von HTTP Requests mit Bash: https://linuxhint.com/curl_bash_examples/