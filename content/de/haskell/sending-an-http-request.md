---
title:                "Versenden einer HTTP Anfrage"
html_title:           "Haskell: Versenden einer HTTP Anfrage"
simple_title:         "Versenden einer HTTP Anfrage"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum 
Warum sollte jemand eine HTTP-Anfrage senden wollen? Es gibt viele Gründe, aber einer der häufigsten ist die Interaktion mit Web-APIs. Durch das Senden von HTTP-Anfragen können wir Daten von anderen Servern abrufen und mit ihnen interagieren, was für viele Anwendungen unerlässlich ist.

## Wie geht es
Um eine HTTP-Anfrage in Haskell zu senden, können wir das "http-client" Paket verwenden. Zuerst müssen wir das Paket importieren: 
```Haskell
import Network.HTTP.Client
```
Als nächstes definieren wir eine `Request` variable, die die URL enthält, an die wir die Anfrage senden möchten:
```Haskell
request <- parseRequest "https://www.beispiel.com"
```
Dann können wir die Anfrage mit der `httpLbs` Funktion ausführen. Diese Funktion führt die Anfrage aus und gibt die Antwort als `Response` zurück:
```Haskell
response <- httpLbs request
```
Schließlich können wir auf die verschiedenen Informationen in der Antwort zugreifen, z.B. den Statuscode, die Header und den Body:
```Haskell
putStrLn $ "Status Code: " ++ show (responseStatusCode response)
putStrLn $ "Header: " ++ show (responseHeaders response)
putStrLn $ "Body: " ++ show (responseBody response)
```
Wenn wir diese Beispiele ausführen, sollten wir eine Antwort von der Beispiel-URL erhalten. 

## Tiefer Tauchen
Wenn Sie sich noch näher mit HTTP-Anfragen in Haskell befassen möchten, können Sie die Dokumentation des "http-client" Pakets durchlesen oder sich mit anderen Paketen und Bibliotheken auseinandersetzen, die das Senden von HTTP-Anfragen erleichtern, wie z.B. "wreq" oder "webcrank". Sie können auch mehr über die verschiedenen HTTP-Methoden und -Codes erfahren, um ein besseres Verständnis dafür zu bekommen, wie HTTP-Anfragen und -Antworten funktionieren.

## Siehe auch
- Dokumentation des "http-client" Pakets: https://hackage.haskell.org/package/http-client
- Dokumentation des "wreq" Pakets: https://hackage.haskell.org/package/wreq
- Dokumentation des "webcrank" Pakets: https://hackage.haskell.org/package/webcrank