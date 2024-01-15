---
title:                "Eine Webseite herunterladen"
html_title:           "Haskell: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit dem Herunterladen einer Webseite beschäftigen? Nun, es kann viele Gründe geben. Vielleicht möchtest du automatisch Daten von einer bestimmten Webseite sammeln oder einen Teil einer Webseite in deiner eigenen Anwendung verwenden. Oder du möchtest einfach lernen, wie du in Haskell Dateien herunterladen kannst.

## So geht's

Um eine Webseite in Haskell herunterzuladen, gibt es einige Schritte zu beachten. Zunächst müssen wir die HTTP-Bibliothek "HTTP.Simple" implementieren, um die Verbindung zur Webseite zu ermöglichen.

```
import Network.HTTP.Simple

```

Als nächstes müssen wir eine Anfrage an die gewünschte URL senden und die Antwort speichern.

```

request <- parseRequest "https://www.example.com"
response <- httpLBS request

```

Jetzt können wir die heruntergeladene Seite im HTML-Format bekommen.

```
let html = getResponseBody response :: ByteString
```

Um die Seite als Text zu erhalten, können wir die Funktion "decodeUtf8" aus der Bibliothek "Data.ByteString.Lazy.Char8" verwenden.

```
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text.Encoding (decodeUtf8)

let text = decodeUtf8 (L8.toStrict html)
```

Schließlich können wir die heruntergeladene Seite ausdrucken.

```

putStrLn text
```

Der gesamte Code sieht also wie folgt aus:

```

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text.Encoding (decodeUtf8)

main :: IO ()
main = do
    request <- parseRequest "https://www.example.com"
    response <- httpLBS request
    let html = getResponseBody response :: ByteString
        text = decodeUtf8 (L8.toStrict html)
    putStrLn text

```

Die Ausgabe sollte ähnlich aussehen wie die Seite, die wir heruntergeladen haben.

## Tiefergehende Informationen

Wenn du tiefer in die Welt des Herunterladens von Webseiten in Haskell eintauchen möchtest, gibt es viele Bibliotheken und Funktionen, die dir dabei helfen können. Zum Beispiel könnte die Bibliothek "Network.HTTP.Conduit" dabei nützlich sein, wenn du authentifizierte Anfragen senden möchtest. Oder du könntest die Bibliothek "Network.Curl" verwenden, um das Herunterladen von Webseiten parallel zu gestalten.

Egal welche Bibliothek du verwendest, es ist wichtig, dass du verstehst, wie HTTP-Anfragen funktionieren und wie du die erhaltenen Daten verarbeiten kannst. Mit genügend Übung wirst du in der Lage sein, jede Webseite erfolgreich herunterzuladen und zu nutzen.

## Siehe auch

- [Haskell Documentation](https://www.haskell.org/documentation/)
- [HTTP.Simple Package on Hackage](https://hackage.haskell.org/package/http-client)
- [Network.HTTP.Conduit Package on Hackage](https://hackage.haskell.org/package/http-conduit)
- [Network.Curl Package on Hackage](https://hackage.haskell.org/package/http-client-curl)