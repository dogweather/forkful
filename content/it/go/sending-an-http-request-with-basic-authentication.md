---
title:                "Go: Invio di una richiesta http con autenticazione di base"
simple_title:         "Invio di una richiesta http con autenticazione di base"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché

In questo post, parleremo di come inviare una richiesta HTTP con autenticazione di base utilizzando il linguaggio di programmazione Go. Questo è un argomento fondamentale per tutti coloro che sviluppano applicazioni web con Go e vogliono garantire un buon livello di sicurezza.

## Come fare

Per inviare una richiesta HTTP con autenticazione di base in Go, dobbiamo utilizzare il pacchetto "net/http" e impostare l'header dell'autorizzazione. Ecco un esempio di codice:

```Go
import (
    "fmt"
    "net/http"
)

func main() {
    client := &http.Client{}

    req, err := http.NewRequest("GET", "https://www.example.com", nil)
    if err != nil {
        fmt.Println(err)
    }

    req.SetBasicAuth("username", "password")

    resp, err := client.Do(req)
    if err != nil {
        fmt.Println(err)
    }

    fmt.Println(resp.Status)
}
```

In questo esempio, stiamo inviando una richiesta HTTP di tipo GET all'URL "https://www.example.com" con l'autenticazione di base impostata con il nome utente e la password appropriati. Possiamo anche usare questo stesso approccio per altri tipi di richieste HTTP, come POST o PUT.

L'esempio di codice sopra ci dà solo l'output dello stato della risposta. Possiamo anche utilizzare il pacchetto "io/ioutil" per leggere il contenuto della risposta e utilizzarlo nel nostro codice come segue:

```Go
import (
    "fmt"
    "io/ioutil"
    "net/http"
)

func main() {
    client := &http.Client{}

    req, err := http.NewRequest("GET", "https://www.example.com", nil)
    if err != nil {
        fmt.Println(err)
    }

    req.SetBasicAuth("username", "password")

    resp, err := client.Do(req)
    if err != nil {
        fmt.Println(err)
    }

    defer resp.Body.Close()

    content, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        fmt.Println(err)
    }

    fmt.Println(string(content))
}
```

## Approfondimento

Per inviare una richiesta HTTP con autenticazione di base in modo sicuro, è necessario utilizzare il protocollo HTTPS anziché HTTP. Ciò garantisce che tutte le informazioni trasmesse siano criptate e quindi meno vulnerabili agli attacchi di hacker.

Inoltre, è importante ricordare che le credenziali di autenticazione di base devono essere sempre crittografate prima di essere inviate. Ciò può essere fatto utilizzando l'algoritmo Base64.

## Vedi anche

Per ulteriori informazioni su come utilizzare il pacchetto "net/http" in Go, puoi consultare la documentazione ufficiale su [golang.org](https://golang.org/pkg/net/http/). Se vuoi approfondire l'argomento dell'autenticazione di base, puoi leggere questo articolo su [MDN](https://developer.mozilla.org/it/docs/Web/HTTP/Authentication).