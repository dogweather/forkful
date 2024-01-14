---
title:                "Fish Shell: Das Senden einer http-Anfrage"
simple_title:         "Das Senden einer http-Anfrage"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

Das Senden von HTTP-Anfragen ist eine nützliche Fähigkeit für alle, die mit Webanwendungen arbeiten. Mit der Fish Shell können Sie ganz einfach HTTP-Anfragen senden und die Antwort verarbeiten.

## Wie es geht

Um eine HTTP-Anfrage mit Fish Shell zu senden, können Sie das integrierte `curl`-Tool verwenden. Hier ist ein Beispiel, wie Sie eine GET-Anfrage an eine API-Endpoint senden können:

```Fish Shell
curl -X GET https://example.com/api/users
```

Dies wird eine Antwort mit dem JSON-Datenformat zurückgeben. Sie können die Antwort direkt in der Shell anzeigen oder in eine Datei schreiben, wie im folgenden Beispiel gezeigt:

```Fish Shell
curl -X GET https://example.com/api/users > users.json
```
Dies speichert die Antwort in einer Datei mit dem Namen "users.json".

Um eine POST-Anfrage mit Daten zu senden, können Sie den `-d` Parameter verwenden, gefolgt von den Daten, die Sie senden möchten. Zum Beispiel:

```Fish Shell
curl -X POST -d 'name=Max&age=28' https://example.com/api/users
```

Dies sendet eine POST-Anfrage an den API-Endpoint mit den angegebenen Daten.

## Tiefergehende Informationen

Die Fish Shell bietet auch die Möglichkeit, benutzerdefinierte HTTP-Anfragen mit dem `http`-Befehl zu senden. Dies ermöglicht eine genauere Kontrolle über die Anfrage und bietet zusätzliche Funktionen wie das Setzen von Anfrage-Headern und das Verarbeiten von Basic-Authentifizierung. Hier ist ein Beispiel, wie Sie eine GET-Anfrage mit `http` senden können:

```Fish Shell
http https://example.com/api/users
```

Dies sendet dieselbe GET-Anfrage wie im vorherigen Beispiel mit `curl`. Weitere Informationen über die Verwendung des `http`-Befehls finden Sie in der [offiziellen Dokumentation](https://fishshell.com/docs/current/commands.html#http).

## Siehe auch

- [Fish Shell Dokumentation](https://fishshell.com/docs/current/)
- [Curl Dokumentation](https://curl.haxx.se/docs/manpage.html)
- [Httpie Dokumentation](https://httpie.org/doc)