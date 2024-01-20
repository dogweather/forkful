---
title:                "Eine HTTP-Anforderung senden"
html_title:           "Bash: Eine HTTP-Anforderung senden"
simple_title:         "Eine HTTP-Anforderung senden"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?
Ein HTTP-Anfrage ist die Art und Weise, wie wir Informationen von Webservern abrufen oder dorthin senden. Programmierer machen das, um Daten aus APIs zu extrahieren, Formulare zu senden, Webseiten zu crawlen und noch viel mehr.

## Wie geht das?
Zur Demonstration verwenden wir `http-conduit`, eine einfache und leistungsfähige Haskell-Bibliothek für HTTP-Anfragen.

```Haskell 
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L

main = do
  simpleHttp "http://example.com" >>= L.writeFile "example.html"
```
Hierbei erstellen und senden wir eine einfache GET-Anfrage zu `http://example.com` und speichern die Antwort als `example.html`.

## Tiefere Einblicke
**1. Historischer Kontext:**  HTTP-Anfragen sind ein fundamentales Konzept des Internets und wurden in den frühen 90er Jahren mit der Entwicklung des World Wide Web populär.

**2. Alternativen:** Es gibt mehrere Bibliotheken, die Sie in Haskell für HTTP-Anfragen verwenden könnten, wie `http-client`, `wreq` und `req`.

**3. Implementierungsdetails:** Der Code „http-conduit“ führt viele Dinge im Hintergrund durch, einschließlich der Verwaltung von Verbindungen, Redirections und SSL/TLS-Verschlüsselung.

## Mehr Informationen
- [http-conduit auf Stackage](https://www.stackage.org/package/http-conduit) 
- [HTTP-Anfrage/Response in Haskell](https://wiki.haskell.org/Web/Libraries)