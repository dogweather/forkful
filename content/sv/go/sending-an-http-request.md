---
title:                "Skicka en http-förfrågan"
date:                  2024-01-20T17:59:56.992184-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skicka en http-förfrågan"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
I Go skickar vi en HTTP-begäran för att prata med webbservrar. Det gör vi för att hämta data, som webbsidor eller API-svar.

## How to:
Installera först `net/http`-paketet ifall det inte redan är gjort:

```Go
import (
    "net/http"
    "io/ioutil"
    "log"
)
```

Skapa en enkel GET-begäran:

```Go
func main() {
    response, err := http.Get("https://example.com")
    if err != nil {
        log.Fatal(err)
    }
    defer response.Body.Close()

    body, err := ioutil.ReadAll(response.Body)
    if err != nil {
        log.Fatal(err)
    }

    println(string(body))
}
```

Kör koden och om allt går som det ska, så ska du se webbsidans innehåll i terminalen.

## Deep Dive
HTTP-begäran härstammar från HTTP (Hypertext Transfer Protocol), skapad för kommunikation på webben sedan 90-talet. I Go används `net/http`-paketet, men det finns också tredjepartsbibliotek som `gorilla/mux` som kan ge fler funktioner.

Alternativ till `ioutil.ReadAll` kan vara att strömma data med `response.Body` direkt för att spara minne vid hantering av stora svar. Detaljer om implementation inkluderar hantering av olika HTTP-metoder som POST, PUT m.fl., samt standarder för headers, statuskoder och hantering av cookies och sessions.

## See Also
- Go's `net/http` dokumentation: https://pkg.go.dev/net/http
- HTTP statuskoder: https://developer.mozilla.org/en-US/docs/Web/HTTP/Status
- Go by Example, HTTP-klienter: https://gobyexample.com/http-clients
- Go standardbiblioteket `ioutil`: https://pkg.go.dev/io/ioutil