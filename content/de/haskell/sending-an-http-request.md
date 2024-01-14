---
title:                "Haskell: Eine http-Anfrage senden."
simple_title:         "Eine http-Anfrage senden."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

Das Senden von HTTP-Anfragen ist eine grundlegende Fähigkeit, die für jede Webanwendung benötigt wird. Es ermöglicht die Kommunikation zwischen einem Client und einem Server und ermöglicht somit den Austausch von Daten und Informationen. In diesem Blogbeitrag werden wir lernen, wie man in Haskell eine HTTP-Anfrage sendet.

## Wie geht das?

Um eine HTTP-Anfrage zu senden, benötigen wir die Bibliothek "http-client" in Haskell. Diese Bibliothek bietet Funktionen und Datentypen, die uns bei der Erstellung und dem Senden von HTTP-Anfragen helfen. Schauen wir uns ein einfaches Beispiel an:

```Haskell
import Network.HTTP.Client

main = do
    request <- parseRequest "GET https://www.example.com"
    manager <- newManager defaultManagerSettings
    response <- httpLbs request manager
    putStrLn $ responseBody response
```

Dieser Code zeigt, wie wir eine einfache GET-Anfrage an die Website www.example.com senden können. Zuerst wird die Funktion "parseRequest" verwendet, um eine Anfrage zu erstellen. Dann wird ein Manager erstellt und die Funktion "httpLbs" verwendet, um die Anfrage zu senden und die Antwort zu erhalten. Schließlich wird die Antwort mit "responseBody" ausgegeben. Dies ist nur ein grundlegendes Beispiel, aber die Bibliothek bietet viele weitere Funktionen, die es uns ermöglichen, komplexere Anfragen zu senden.

## Tiefer eintauchen

Um eine bessere Vorstellung davon zu bekommen, wie HTTP-Anfragen funktionieren, ist es hilfreich, sich mit den verschiedenen Komponenten einer Anfrage vertraut zu machen. Eine HTTP-Anfrage besteht aus einem Befehl (wie GET oder POST), einer URL, optionalen Headern und einem Body. Mit der Bibliothek "http-client" können wir diese Komponenten einzeln manipulieren, um eine maßgeschneiderte Anfrage zu erstellen.

Zusätzlich zur "http-client" Bibliothek gibt es auch andere nützliche Tools wie "http-conduit" und "wreq", die beim Senden von HTTP-Anfragen in Haskell helfen können. Es ist wichtig, sich mit diesen Tools vertraut zu machen, um das Beste aus Ihrer Anwendung herauszuholen.

## Siehe auch

- [http-client Dokumentation](https://hackage.haskell.org/package/http-client)
- [http-conduit Dokumentation](https://hackage.haskell.org/package/http-conduit)
- [wreq Dokumentation](https://hackage.haskell.org/package/wreq)