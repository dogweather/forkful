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

## Cosa & Perché?
In poche parole, inviare una richiesta HTTP significa comunicare con un server web per ottenere informazioni o inviare dati. I programmatori lo fanno per accedere a risorse online, come siti web o API, e per creare applicazioni che si collegano a Internet.

## Come fare:
Ecco un esempio di come inviare una richiesta HTTP in Go:

```
risposta, errore := http.Get("https://www.ilmiosito.com")

if errore != nil {
    fmt.Println("Errore:", errore)
}

fmt.Println(risposta.StatusCode) // 200
```

Questo codice utilizza la funzione `http.Get()` per ottenere la risposta dall'URL specificato e la memorizza nella variabile `risposta`. In caso di errore, viene stampato un messaggio di errore. La risposta viene poi stampata con il suo codice di stato.

## Approfondimento:
Invio di richieste HTTP è un'attività comune nella programmazione web e Go offre una sintassi semplice e potente per farlo. Tuttavia, ci sono anche alcune alternative, come l'utilizzo del pacchetto `net/http` o di librerie esterne come `Gorilla HTTP` o `go-httpclient`.

Per implementare correttamente l'invio di una richiesta HTTP, è importante comprendere i diversi metodi e parametri disponibili. Ad esempio, è possibile specificare un corpo della richiesta, header personalizzati e gestire gli errori in modo appropriato.

## Vedi anche:
- [Package http](https://golang.org/pkg/net/http/)
- [Gorilla HTTP](https://github.com/gorilla/http)
- [Go-httpclient](https://github.com/mreiferson/go-httpclient)