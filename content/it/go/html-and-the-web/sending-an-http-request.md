---
title:                "Inviare una richiesta HTTP"
aliases: - /it/go/sending-an-http-request.md
date:                  2024-02-03T18:08:32.744521-07:00
model:                 gpt-4-0125-preview
simple_title:         "Inviare una richiesta HTTP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/sending-an-http-request.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Inviare una richiesta HTTP comporta l'inizializzazione di una chiamata dalla tua applicazione Go a un server web, un'API o qualsiasi altro servizio basato su HTTP. I programmatori fanno questo per interagire con risorse web, recuperare dati, inviare moduli o comunicare con altri servizi attraverso internet.

## Come fare:

In Go, inviare una richiesta HTTP e gestire la risposta comporta l'utilizzo del pacchetto `net/http`. Ecco un esempio passo dopo passo su come inviare una semplice richiesta GET e leggere la risposta:

```go
package main

import (
    "fmt"
    "io/ioutil"
    "log"
    "net/http"
)

func main() {
    // Definire l'URL della risorsa
    url := "http://example.com"

    // Usare http.Get per inviare la richiesta GET
    resp, err := http.Get(url)
    if err != nil {
        log.Fatal(err)
    }
    // Chiudere il corpo della risposta quando la funzione termina
    defer resp.Body.Close()

    // Leggere il corpo della risposta
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        log.Fatal(err)
    }

    // Convertire il corpo della risposta in una stringa e stamparlo
    fmt.Println(string(body))
}
```

Output di esempio (abbreviato per brevità):
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

Per inviare una richiesta POST con dati di un modulo, è possibile usare `http.PostForm`:

```go
package main

import (
    "fmt"
    "io/ioutil"
    "net/http"
    "net/url"
)

func main() {
    // Definisci l'URL e i dati del modulo
    url := "http://example.com/form"
    data := url.Values{}
    data.Set("key", "value")

    // Invia la richiesta POST con i dati del modulo
    resp, err := http.PostForm(url, data)
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()

    // Leggi e stampa la risposta
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        panic(err)
    }

    fmt.Println(string(body))
}
```

## Approfondimento

Il pacchetto `net/http` in Go fornisce un modo potente e flessibile per interagire con i server HTTP. Il suo design riflette l'attenzione di Go per la semplicità, l'efficienza e la robustezza. In origine, funzionalità come la gestione dei payload JSON o XML richiedevano la creazione manuale del corpo della richiesta e l'impostazione degli header appropriati. Man mano che Go si è evoluto, la comunità ha sviluppato pacchetti di livello superiore che semplificano ulteriormente questi compiti, come `gorilla/mux` per il routing e `gjson` per la manipolazione JSON.

Un aspetto notevole del client HTTP di Go è il suo uso di interfacce e strutture, come `http.Client` e `http.Request`, che consentono un'ampia personalizzazione e test. Ad esempio, è possibile modificare `http.Client` per far scadere le richieste o mantenere vive le connessioni per migliorare le prestazioni.

Un'alternativa considerata per interazioni HTTP più semplici è l'uso di librerie di terze parti come "Resty" o "Gentleman". Questi pacchetti offrono un'astrazione di livello più alto per le richieste HTTP, rendendo i compiti comuni più concisi. Tuttavia, comprendere e utilizzare il pacchetto `net/http` sottostante è cruciale per affrontare scenari di interazione HTTP più complessi o unici, fornendo una base su cui le funzionalità di concorrenza di Go e la potente libreria standard possono essere pienamente sfruttate.
