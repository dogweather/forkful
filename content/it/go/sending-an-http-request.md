---
title:                "Inviare una richiesta http"
html_title:           "Go: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Perché

Se stai sviluppando un'applicazione web o un servizio, inviare richieste HTTP è un'operazione fondamentale. Con Go, questo processo è estremamente facile da gestire grazie alla sua sintassi semplice e alla presenza di librerie integrate per le richieste HTTP.

## Come fare

Per inviare una richiesta HTTP in Go, dobbiamo prima importare il pacchetto "net/http". Successivamente, possiamo utilizzare la funzione `Get()` di questo pacchetto per inviare una richiesta GET a un URL specifico. Di seguito è riportato un esempio di codice:

```Go
package main

import (
    "fmt"
    "net/http"
)

func main() {
    res, err := http.Get("https://example.com")

    if err != nil {
        fmt.Println(err)
        return
    }

    defer res.Body.Close()

    fmt.Println(res.Status)
}
```

L'output di questo codice sarà "200 OK", che è lo stato di risposta di default di una richiesta GET valida. Possiamo anche passare dei parametri alla funzione `Get()`, come ad esempio un header con informazioni sull'utente o dei dati da inviare nel corpo della richiesta. 

## Approfondimento

Oltre alla semplice funzione `Get()`, Go fornisce anche le funzioni `Post()`, `Put()` e `Delete()` per inviare rispettivamente richieste POST, PUT e DELETE a un server. Inoltre, è possibile configurare i parametri delle richieste, come ad esempio impostare un limite di tempo per la risposta o specificare un proxy da utilizzare.

Per ulteriori informazioni su come gestire le richieste HTTP in Go, si consiglia di consultare la documentazione ufficiale del pacchetto "net/http" e gli esempi presenti sul sito del linguaggio.

## Vedi anche

- Documentazione ufficiale sul pacchetto "net/http": https://golang.org/pkg/net/http/
- Esempi di richieste HTTP in Go: https://golang.org/doc/tutorial/web-service-gin