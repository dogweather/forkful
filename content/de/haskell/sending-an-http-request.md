---
title:                "Das Senden einer http-Anfrage"
html_title:           "Haskell: Das Senden einer http-Anfrage"
simple_title:         "Das Senden einer http-Anfrage"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Absenden einer HTTP-Anfrage ist ein grundlegender Prozess in der Webprogrammierung, bei dem ein Computer eine Anfrage an einen Server sendet, um eine Ressource abzurufen oder zu modifizieren. Programmierer nutzen HTTP-Anfragen, um Daten von APIs zu erhalten, mit Webbrowsern zu interagieren und vieles mehr.

## So geht's:
Um eine HTTP-Anfrage in Haskell zu senden, müssen wir das `Network.HTTP`-Paket importieren. Dann können wir die `simpleHTTP`-Funktion verwenden, um eine Anfrage an eine URL zu senden und die Antwort als Objekt zu erhalten.

```Haskell
-- importieren des `Network.HTTP` Pakets
import Network.HTTP

-- senden einer GET-Anfrage an die URL "http://www.google.com"
simpleHTTP (getRequest "http://www.google.com") >>= getResponseBody
```

Die obige Codezeile würde die HTML-Seite von Google zurückgeben. Wir können auch eine benutzerdefinierte Anfrage erstellen, indem wir verschiedene Funktionen wie `addRequestHeader` verwenden, um Header hinzuzufügen, oder `setRequestBody` für die POST-Anfrage.

## Tiefere Einblicke:
HTTP wurde in den 1990er Jahren entwickelt und ist weiterhin das am häufigsten verwendete Protokoll für die Kommunikation im Web. Eine alternative Option dazu ist HTTPS, welches eine verschlüsselte Verbindung bietet. Haskell bietet auch das `Network.HTTPs`-Paket für sichere Anfragen.

Bei der Implementierung einer HTTP-Anfrage in Haskell wird die `IO`-Monade verwendet, da das Senden einer Anfrage eine Operation ist, von der externer Code und Zustand abhängen können. Zudem gibt es auch andere nützliche Funktionen in `Network.HTTP`, wie z.B. `urlEncode`.

## Siehe auch:
- [Haskell Dokumentation zum Network.HTTP-Paket](https://www.stackage.org/haddock/ltl-tutorial/Network-HTTP.html)
- [HTTP vs. HTTPS: Was ist der Unterschied?](https://www.cloudflare.com/learning/ssl/http-vs-https/)
- [Einführung in die Monaden in Haskell](https://en.wikibooks.org/wiki/Haskell/Understanding_monads)