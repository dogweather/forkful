---
title:                "Go: Scaricare una pagina web."
simple_title:         "Scaricare una pagina web."
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché

Scaricare una pagina web è un'attività essenziale per molti programmatori, specialmente quelli che lavorano nel campo della creazione di applicazioni web. Con Go, il linguaggio di programmazione open source sviluppato da Google, è possibile automatizzare questo processo in modo semplice ed efficiente.

## Come fare

Per scaricare una pagina web utilizzando Go, è necessario utilizzare il pacchetto `net/http`. Questo pacchetto fornisce una serie di funzioni per effettuare richieste HTTP e ottenere il contenuto di una pagina web.

```Go
import (
    "fmt"
    "net/http"
)

func main() {
    // Effettua una richiesta GET alla pagina web specificata
    response, err := http.Get("https://nome-sito-web.com")

    // Gestisce eventuali errori
    if err != nil {
        fmt.Println("Errore durante la richiesta:", err)
    }

    // Stampa il contenuto della pagina scaricata
    fmt.Println("Contenuto della pagina:", response.Body)
    
    // Chiude il corpo della risposta HTTP
    defer response.Body.Close()
}
```

## Approfondimento

Quando si effettua una richiesta HTTP utilizzando Go, è importante tenere presente che la risposta può essere ricevuta in più parti. Per gestire questa situazione, è possibile utilizzare il pacchetto `io` per concatenare le diverse parti della risposta:

```Go
import (
    "fmt"
    "io"
    "io/ioutil"
    "net/http"
)

func main() {
    response, err := http.Get("https://nome-sito-web.com")

    if err != nil {
        fmt.Println("Errore durante la richiesta:", err)
    }

    // Utilizza il pacchetto io per concatenare le parti della risposta
    body, err := ioutil.ReadAll(response.Body)
    if err != nil {
        fmt.Println("Errore durante la lettura della risposta:", err)
    }

    fmt.Println("Contenuto della pagina:", string(body))

    defer response.Body.Close()
}
```

Altre informazioni sui pacchetti `net/http` e `io` possono essere trovate nella documentazione ufficiale di Go.

## Vedi anche

- [Documentazione ufficiale di Go](https://golang.org/pkg)
- [Tutorial su come effettuare richieste HTTP con Go](https://www.smashingmagazine.com/2021/04/http-requests-in-go/)
- [Alcuni esempi di utilizzo del pacchetto `net/http`](https://golangbyexample.com/http-get-go/)