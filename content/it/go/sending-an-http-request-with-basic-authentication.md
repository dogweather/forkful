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

## Perché

Se stai lavorando con API o servizi web che richiedono la verifica delle credenziali, potresti dover inviare una richiesta HTTP con l'autenticazione di base. Questo articolo ti guiderà nel processo di coding in Go per inviare una richiesta HTTP con l'autenticazione di base.

## Come fare

Per inviare una richiesta HTTP con l'autenticazione di base in Go, puoi seguire questi semplici passaggi:

1. Importa il pacchetto `net/http` per gestire le richieste HTTP.
2. Costruisci un client HTTP utilizzando la funzione `http.Client()`.
3. Crea una richiesta HTTP usando la funzione `http.NewRequest()`, specificando il metodo di richiesta, l'URL e, se necessario, i dati del corpo e gli header di autenticazione.
4. Utilizza la funzione `SetBasicAuth()` per impostare le credenziali per l'autenticazione di base.
5. Invia la richiesta utilizzando il client creato e gestisci la risposta.

Ecco un esempio di come sarebbe il codice per inviare una richiesta GET con l'autenticazione di base:

```Go
import (
	"fmt"
	"net/http"
	"encoding/base64"
)

func main() {

	// Creazione del client HTTP
	client := http.Client{}

	// Creazione della richiesta HTTP con l'autenticazione di base
	req, err := http.NewRequest("GET", "https://example.com/api", nil)
	if err != nil {
		fmt.Println(err)
	}
	req.SetBasicAuth("username", "password")

	// Invio della richiesta HTTP
	resp, err := client.Do(req)
	if err != nil {
		fmt.Println(err)
	}
	defer resp.Body.Close()

	// Lettura della risposta
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		fmt.Println(err)
	}
	
	// Stampa della risposta
	fmt.Println(string(body))
}
```

Ecco un esempio di output se la richiesta viene eseguita con successo:

```html
<html>
	<head>
		<title>Pagina di esempio</title>
	</head>
	<body>
		<h1>Benvenuto!</h1>
	</body>
</html>
```

## Approfondimenti

Oltre alla funzione `SetBasicAuth()`, esistono anche alcune altre opzioni per gestire l'autenticazione di base nelle richieste HTTP in Go. Puoi utilizzare la funzione `SetAuthHeader()`, che ti permette di specificare manualmente l'header di autenticazione, oppure puoi utilizzare la libreria di terze parti "httpauth" che semplifica l'aggiunta di autenticazione alle tue richieste HTTP.

## Vedi anche

- Documentazione ufficiale del pacchetto `net/http`: https://golang.org/pkg/net/http/
- Libreria di terze parti "httpauth": https://github.com/goji/httpauth