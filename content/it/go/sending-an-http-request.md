---
title:                "Go: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Perché

La convalida delle richieste HTTP è fondamentale per garantire che le informazioni vengano trasferite correttamente tra client e server. Saper inviare una richiesta HTTP è fondamentale per qualsiasi sviluppatore Go.

## Come fare

Per inviare una richiesta HTTP in Go, possiamo utilizzare la libreria standard `net/http`. Prima di tutto, dobbiamo creare un oggetto `http.Client` utilizzando la funzione `NewClient`:

```Go
client := &http.Client{}
```

Successivamente, creeremo un oggetto `http.Request` specificando il metodo di richiesta, l'URL di destinazione e, se necessario, il corpo della richiesta:

```Go
req, err := http.NewRequest("GET", "https://example.com", nil)
```

Infine, possiamo utilizzare il nostro client per inviare la richiesta e ottenere la risposta:

```Go
resp, err := client.Do(req)
if err != nil {
    // gestione dell'errore
}
defer resp.Body.Close()
```

La risposta contiene informazioni come lo status code, l'header e il corpo della risposta. Possiamo utilizzarli per elaborare le informazioni ricevute dal server.

```Go
fmt.Println("Status code:", resp.StatusCode)
fmt.Println("Header:", resp.Header)
body, err := ioutil.ReadAll(resp.Body)
if err != nil {
    // gestione dell'errore
}
fmt.Println("Body:", string(body))
```

## Approfondimento

Inviare una richiesta HTTP in Go può rivelarsi più complesso a seconda delle specifiche esigenze. Ad esempio, è possibile specificare parametri di query, header personalizzati, autenticazione e altro ancora. La libreria `net/http` offre molte funzionalità avanzate per gestire questi scenari.

Inoltre, possiamo utilizzare anche altre librerie di terze parti come `gorilla/mux` per gestire il routing delle richieste e `golang.org/x/oauth2` per gestire l'autenticazione OAuth 2.0.

## Vedi anche

- Documentazione ufficiale delle librerie https://golang.org/pkg/net/http/
- Guida dettagliata su come inviare richieste HTTP in Go https://www.codementor.io/codehakase/building-a-restful-api-with-golang-a6yivzqdo
- Esempi pratici di utilizzo delle librerie `gorilla/mux` e `golang.org/x/oauth2` https://medium.com/@masnun/making-http-requests-in-golang-dd123379efe7