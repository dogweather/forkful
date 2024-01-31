---
title:                "Scaricare una pagina web"
date:                  2024-01-20T17:44:02.218035-07:00
model:                 gpt-4-1106-preview
simple_title:         "Scaricare una pagina web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Scaricare una pagina web significa prelevare i dati da un sito internet e salvarli localmente. I programmatori lo fanno per analizzare contenuti, testare disponibilità o raccogliere dati.

## Come Fare:
Ecco un esempio semplice in Go per scaricare una pagina web:

```Go
package main

import (
	"fmt"
	"io"
	"net/http"
	"os"
)

func main() {
	// URL della pagina da scaricare
	url := "http://example.com"

	// Richiesta HTTP
	response, err := http.Get(url)
	if err != nil {
		fmt.Println(err)
		return
	}
	defer response.Body.Close()

	// Creazione del file locale
	outFile, err := os.Create("example.html")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer outFile.Close()

	// Copia del contenuto nel file
	_, err = io.Copy(outFile, response.Body)
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Println("Pagina scaricata con successo!")
}
```

Esempio di output:
```
Pagina scaricata con successo!
```

## Approfondimento:
Un tempo, scaricare pagine web era più comune per costruire archivi o analizzare il SEO. Ora, con le API, è spesso meglio interfacciarsi direttamente con i dati forniti. Nel caso di Go, `net/http` è il pacchetto standard per chiamate HTTP. L’uso di `io.Copy` è efficiente per grandi quantità di dati perché non carica tutto in memoria. Se usi Go in contesti come lo scraping o i test di servizi web, conoscere questa funzionalità è essenziale.

## Vedi Anche:
- Documentazione ufficiale del pacchetto `net/http` per Go: [https://pkg.go.dev/net/http](https://pkg.go.dev/net/http)
- Go by Example - HTTP Clients: [https://gobyexample.com/http-clients](https://gobyexample.com/http-clients)
