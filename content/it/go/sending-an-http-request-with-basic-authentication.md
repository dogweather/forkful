---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Go: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Invio di una richiesta HTTP con autenticazione di base è quando un programma comunica con un server web e fornisce credenziali di accesso per accedere alle risorse protette. I programmatori lo fanno per garantire una maggiore sicurezza nelle comunicazioni tra il loro programma e il server.

## Come fare:

Go rende facile l'invio di una richiesta HTTP con autenticazione di base utilizzando la libreria "net/http". Di seguito è riportato un esempio di codice che mostra come inviare una richiesta GET con autenticazione di base a un server:

```
package main

import (
    "fmt"
    "net/http"
    "io/ioutil"
)

func main() {
    url := "https://example.com/api"
    username := "username"
    password := "password"
    
    req, err := http.NewRequest("GET", url, nil)
    if err != nil {
        panic(err)
    }

    req.SetBasicAuth(username, password)

    resp, err := http.DefaultClient.Do(req)
    if err != nil {
        panic(err)
    }

    defer resp.Body.Close()
    
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        panic(err)
    }
    
    fmt.Println(string(body))
}
```
Il codice sopra invia una richiesta GET all'URL specificato con autenticazione di base utilizzando il metodo ```http.NewRequest```. Vengono quindi impostati username e password con il metodo ```req.SetBasicAuth```. Infine, viene effettivamente eseguita la richiesta al server e il corpo della risposta viene letto e stampato sulla console.

L'output dovrebbe essere qualcosa del genere:

```
{"message": "Autenticazione riuscita. Benvenuto!"}
```

## Approfondimento:

L'autenticazione di base è uno dei metodi più antichi per la sicurezza delle comunicazioni web. È stato introdotto nei primi standard HTTP nel 1995 e si basa su un semplice schema di autenticazione username e password inviato come testo non criptato. Ciò lo rende un metodo di autenticazione molto semplice ma anche meno sicuro rispetto ad altre alternative come OAuth o token JWT.

Una alternativa all'autenticazione di base in Go è l'utilizzo della libreria "golang.org/x/oauth2" per implementare l'autenticazione OAuth con un server API.

## Vedi anche:

Per ulteriori informazioni su come inviare richieste HTTP con autenticazione di base in Go, si consiglia di consultare la documentazione ufficiale della libreria "net/http" e "golang.org/x/oauth2".