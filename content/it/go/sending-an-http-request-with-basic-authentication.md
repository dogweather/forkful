---
title:                "Inviare una richiesta http con autenticazione di base"
date:                  2024-01-20T18:02:00.653890-07:00
model:                 gpt-4-1106-preview
simple_title:         "Inviare una richiesta http con autenticazione di base"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
Inviare una richiesta HTTP con autenticazione di base significa inserire username e password in una richiesta HTTP per accedere a risorse protette. I programmatori lo fanno per interagire con API che richiedono una forma di identificazione sicura.

## How to:
Per inviare una richiesta HTTP con autenticazione di base in Go, usa il modulo "net/http". Ecco un esempio:

```Go
package main

import (
	"encoding/base64"
	"fmt"
	"net/http"
)

func main() {
	client := &http.Client{}
	req, err := http.NewRequest("GET", "http://example.com/resource", nil)
	if err != nil {
		panic(err)
	}

	username := "usuario"
	password := "contraseña"
	basicAuth := "Basic " + base64.StdEncoding.EncodeToString([]byte(username + ":" + password))
	req.Header.Add("Authorization", basicAuth)

	resp, err := client.Do(req)
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	fmt.Println("Status Code:", resp.StatusCode)
	if resp.StatusCode == http.StatusOK {
		fmt.Println("Autenticazione riuscita!")
	} else {
		fmt.Println("Autenticazione fallita.")
	}
}
```

Esempio di output:

```
Status Code: 200
Autenticazione riuscita!
```

## Deep Dive:
Prima del 1996, le credenziali potevano essere inviate in chiaro. L'autenticazione di base, standardizzata dall'RFC 2617, ha aggiunto un livello di sicurezza criptando le credenziali con Base64. Però, Base64 non è crittografia e quindi la comunicazione via HTTPS è raccomandata per proteggere le credenziali.

Le alternative all'autenticazione di base includono OAuth, token di accesso e autenticazione a due fattori. Scegliendo la metodologia più adatta, valuta la facilità d'uso, la sicurezza e le esigenze specifiche dell'app.

Nei dettagli implementativi, fare attenzione che le password non siano inserite direttamente nel codice, ma gestite tramite variabili d'ambiente o sistemi di gestione segreta, specialmente in ambienti di produzione.

## See Also:
- Documentazione Go per il modulo "net/http": https://pkg.go.dev/net/http
- RFC 7617, "The 'Basic' HTTP Authentication Scheme": https://tools.ietf.org/html/rfc7617
- Una guida all'autenticazione HTTPS in Go: https://golangdocs.com/golang-http-client
- Sicurezza e gestione delle credenziali nelle applicazioni Go: https://blog.golang.org/security
