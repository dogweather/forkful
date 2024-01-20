---
title:                "Inviare una richiesta http"
html_title:           "C++: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché? 
Inviare una richiesta HTTP significa comunicare con un server web. I programmatori lo fanno per interagire con i servizi e le API basate sul web.

## Come Fare:
Ecco un semplice esempio di invio di una richiesta GET in Go.

```Go
package main

import (
	"fmt"
	"net/http"
	"io/ioutil"
)

func main() {
	richiesta, err := http.Get("http://example.com")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer richiesta.Body.Close()
	body, err := ioutil.ReadAll(richiesta.Body)
	if err != nil {
		fmt.Println(err)
		return
	}
	fmt.Println(string(body))
}
```

In questo esempio, si ottiene la risposta HTTP dal server e si stampa il corpo della risposta.

## Approfondisci:

Inviare richieste HTTP è una pratica fondamentale nella programmazione web. Il protocollo HTTP è stato introdotto per la prima volta nel 1991 e da allora è diventato lo standard per la comunicazione tra client e server.

Esistono molte librerie alternative in Go per inviare richieste HTTP, come fasthttp, che offre prestazioni migliorate sotto carichi pesanti, o gorequest, che offre un'API più facile da usare.

Quando invii una richiesta HTTP in Go, in realtà stai creando una struttura `http.Request`, che contiene dettagli come l'URL, il metodo di richiesta (GET, POST, ecc.), gli headers della richiesta e il corpo della richiesta. Questa struttura viene poi passata al metodo `http.Client.Do`, che spedisce la richiesta al server e restituisce una struttura `http.Response`.

## Vedi Anche: 
1. [Introduzione a HTTP](https://developer.mozilla.org/it/docs/Web/HTTP/Overview)
2. [Libreria net/http](https://golang.org/pkg/net/http/)
3. [Fasthttp](https://github.com/valyala/fasthttp)
4. [Gorequest](https://github.com/parnurzeal/gorequest)