---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Bash: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Invio di una richiesta HTTP con autenticazione base in Go

## Cosa & Perché?
L'invio di una richiesta HTTP con autenticazione base significa spedire dati ad un servizio web protetto da un semplice sistema di autenticazione. Questa pratica è comune quando si lavora con servizi web restrittivi che richiedono una prova della tua identità per concederti l'accesso.

## Come fare:
Ecco un esempio di codice, in Go, per inviare una richiesta GET con autenticazione base:
```Go
package main

import (
        "net/http"
        "fmt"
)

func main() {
    client := &http.Client{}
    req, err := http.NewRequest("GET", "http://example.com", nil)
    if err != nil {
        fmt.Println(err)
        return
    }
    req.SetBasicAuth("username", "password")
    resp, err := client.Do(req)
    if err != nil {
        fmt.Println(err)
        return
    }
    defer resp.Body.Close()
    // ...
}
```
In questo esempio, stiamo creando una nuova richiesta GET, settando l'autenticazione base con `req.SetBasicAuth("username", "password")`, inviando poi la richiesta con `client.Do(req)`.

## Approfondimento
L’autenticazione base è un meccanismo di autenticazione molto semplice, introdotto in HTTP 1.0 e ancora utilizzato. Tuttavia, essendo una tecnica molto semplice che invia nome utente e password in testo non cifrato (sebbene codificati in base64), viene ora consigliato utilizzare metodi più sicuri, come la autenticazione token-based o l’OAuth.

Nell'implementazione nell'esempio di codice, è importante notare come la funzione Do del client HTTP utilizzi l'autenticazione in maniera sincrona. Se si vuole rendere l'operazione asincrona, sarà necessario utilizzare un meccanismo di goroutine e di channel.

## Approfondisci
Per ulteriori approfondimenti sul lavoro con le API HTTP in Go, risorse:
- [HTTP package - The Go Programming Language](https://golang.org/pkg/net/http/)
- [Making HTTP Requests in Golang](https://medium.com/@masnun/making-http-requests-in-golang-dd123379efe7)