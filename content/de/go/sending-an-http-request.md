---
title:                "Eine HTTP-Anforderung senden"
html_title:           "Bash: Eine HTTP-Anforderung senden"
simple_title:         "Eine HTTP-Anforderung senden"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?

Ein HTTP Request ist ein Nachrichtenaustausch zwischen Client und Server im World Wide Web. Programmierer verwenden es, um Daten von einer Web-API abzurufen oder Daten an sie zu senden.

## So geht's:

Hier haben wir ein einfaches Codebeispiel, das ein HTTP GET Request an eine Website macht.

```Go
package main

import (
    "io/ioutil"
    "log"
    "net/http"
)

func main() {
    res, err := http.Get("http://www.google.de")
    if err != nil {
        log.Fatal(err)
    }
    responseBody, err := ioutil.ReadAll(res.Body)
    if err != nil {
        log.Fatal(err)
    }
    defer res.Body.Close()
    log.Println(string(responseBody))
}
```

Wenn Sie dieses ausführen, erhalten Sie die HTML-Antwort der Website in Ihrem Console-Log.

## Vertiefung

Historisch gesehen, wurde die HTTP-Anforderung ursprünglich in der Version 0.9 von HTTP eingeführt, die 1991 veröffentlicht wurde. In Go, können wir verschiedene Arten HTTP-Requests verwenden, einschließlich GET, POST, PUT und DELETE. Es gibt mehrere Go-Pakete zur Verfügung, um HTTP-Requests und -Anforderungen durchzuführen, einschließlich net/http und httpclient.

Ein interessanter Aspekt der HTTP-Anfragen in Go ist die Kontrolle über Timeouts. Standardmäßig bietet der http.Client von Go keine Timeouts und das kann zu unerwarteten Ergebnissen führen. Daher ist es empfehlenswert, immer einen Timeout zu setzen.

Ein alternatives Paket zur Durchführung von HTTP-Anfragen ist das "resty"-Paket. Es bietet viele zusätzliche Funktionen wie automatisches Retry, Cookie-Unterstützung und viele mehr. 

## Siehe auch

Für mehr Details, checken Sie bitte folgende Ressourcen:

1. Go Dokumentation für net/http: https://golang.org/pkg/net/http/
2. Artikel darüber, wie man HTTP-Anfragen in Go gestaltet: https://medium.com/@masnun/making-http-requests-in-golang-dd123379efe7
3. Das Resty-Paket für Go: https://github.com/go-resty/resty
4. Artikel über die Geschichte von HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/Evolution_of_HTTP