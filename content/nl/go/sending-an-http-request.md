---
title:                "Een HTTP-verzoek verzenden"
date:                  2024-01-28T22:07:41.702237-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een HTTP-verzoek verzenden"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/sending-an-http-request.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het verzenden van een HTTP-verzoek is hoe je programma een ander systeem vraagt om gegevens of gegevens naar het verzendt. Programmeurs doen dit om te interageren met webservices, API's en om informatie uit te wisselen via het internet.

## Hoe:
Hier is een fragment in Go voor het verzenden van een GET-verzoek en het verwerken van de respons:

```Go
package main

import (
	"io"
	"log"
	"net/http"
	"os"
)

func main() {
	response, err := http.Get("https://api.example.com/data")
	if err != nil {
		log.Fatal(err)
	}
	defer response.Body.Close()

	if response.StatusCode == http.StatusOK {
		body, readErr := io.ReadAll(response.Body)
		if readErr != nil {
			log.Fatal(readErr)
		}
		os.Stdout.Write(body)
	} else {
		log.Printf("Niet-OK response status ontvangen: %s", response.Status)
	}
}
```

Dit is wat je mogelijk ziet na het uitvoeren hiervan:

```
{"name":"John Doe","occupation":"Software Developer"}
```

## Diepgaande analyse

Voordat Go's `net/http`-pakket het leven makkelijker maakte, was het verzenden van HTTP-verzoeken lastig. In de begindagen hielden we ons bezig met low-level socketprogrammering dat veel ging over het handmatig beheren van TCP-verbindingen en -protocollen. Vandaag de dag abstraheert de standaardbibliotheek deze complexiteiten.

Hoewel `http.Get` handig is voor eenvoudige verzoeken, wanneer je meer controle nodig hebt, zijn `http.NewRequest` en `http.Client` je vrienden. Ze laten je headers wijzigen, timeouts instellen, en redirects nauwkeuriger beheren.

Een punt om over na te denken: `http.Get` en zijn vrienden zijn blocking calls. Ze retourneren niet tot de HTTP-respons volledig is ontvangen. Gebruik in een app met veel verkeer de gelijktijdigheidsfuncties van Go, zoals goroutines en kanalen, om vertraging te voorkomen.

Alternatieven zijn third-party pakketten zoals `Resty` of `GoReq`. Sommigen geven de voorkeur aan deze vanwege hun vloeiende interfaces en extra functionaliteit. Overweeg altijd of de voordelen opwegen tegen de kosten van het toevoegen van een afhankelijkheid.

## Zie ook

- De Go net/http-pakketdocumentatie: [https://golang.org/pkg/net/http/](https://golang.org/pkg/net/http/)
- Effectief Go – Gelijktijdigheid: [https://golang.org/doc/effective_go#concurrency](https://golang.org/doc/effective_go#concurrency)
- Go bij Voorbeeld – HTTP-clients: [https://gobyexample.com/http-clients](https://gobyexample.com/http-clients)
- Het boek "De Go Programmeertaal" voor een diepgaand begrip van Go's standaardbibliotheek.
