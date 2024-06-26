---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:10.998048-07:00
description: "Hoe: Om een HTTP-verzoek met basisauthenticatie in Go te maken, moet\
  \ je je verzoekheaders zo opstellen dat ze het `Authorization` veld bevatten, gevuld\u2026"
lastmod: '2024-03-13T22:44:50.287136-06:00'
model: gpt-4-0125-preview
summary: Om een HTTP-verzoek met basisauthenticatie in Go te maken, moet je je verzoekheaders
  zo opstellen dat ze het `Authorization` veld bevatten, gevuld met je inloggegevens
  in het juiste formaat.
title: Een HTTP-verzoek verzenden met basisauthenticatie
weight: 45
---

## Hoe:
Om een HTTP-verzoek met basisauthenticatie in Go te maken, moet je je verzoekheaders zo opstellen dat ze het `Authorization` veld bevatten, gevuld met je inloggegevens in het juiste formaat. Hieronder vind je een voorbeeld dat laat zien hoe je een GET-verzoek naar een API-eindpunt stuurt dat basisauthenticatie vereist:

```go
package main

import (
	"fmt"
	"net/http"
	"encoding/base64"
)

func main() {
	client := &http.Client{}
	req, err := http.NewRequest("GET", "http://example.com/api/data", nil)
	if err != nil {
		panic(err)
	}

	username := "yourUsername"
	password := "yourPassword"
    // Codeer inloggegevens
	auth := base64.StdEncoding.EncodeToString([]byte(username + ":" + password))
    // Stel Authorization header in
	req.Header.Add("Authorization", "Basic " + auth)

	resp, err := client.Do(req)
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	fmt.Println("Reactie status:", resp.Status)
}
```

Als je deze code uitvoert, wordt er een GET-verzoek naar de opgegeven URL gestuurd met de noodzakelijke Authorization-header. De output ziet er ongeveer zo uit, afhankelijk van je eindpunt en service:

```
Reactie status: 200 OK
```

## Diepgaand
Basisauthenticatie in HTTP-verzoeken is een breed ondersteunde methode voor het afdwingen van toegangscontroles tot webbronnen. Het stuurt simpelweg een gebruikersnaam en wachtwoord met elk verzoek mee, wat het eenvoudig te implementeren maakt maar niet de meest veilige methode die beschikbaar is. Een groot nadeel is dat de inloggegevens in duidelijke tekst worden verzonden (aangezien Base64 eenvoudig te decoderen is), tenzij het in combinatie met SSL/TLS wordt gebruikt. Dit kan gevoelige informatie blootstellen aan Man-in-the-Middle-aanvallen.

In Go houdt het versturen van deze verzoeken in dat je de `Authorization`-header rechtstreeks manipuleert. Hoewel de standaardbibliotheek van Go (`net/http`) krachtige primitieven biedt voor het omgaan met HTTP(s)-communicatie, is het relatief laag niveau, wat vereist dat ontwikkelaars handmatig verschillende aspecten van HTTP-verzoek-/responsafhandeling behandelen. Dit geeft programmeurs veel flexibiliteit maar betekent ook dat men nauwer moet letten op beveiligingsimplicaties, codering en correct beheer van headers.

Voor toepassingen die hogere beveiliging vereisen, moeten geavanceerdere authenticatiesystemen zoals OAuth2 of JWT (JSON-webtokens) worden overwogen. Deze benaderingen bieden robuustere beveiligingsfuncties en worden breed ondersteund in moderne API's en diensten. De groeiende ecosysteem van Go omvat tal van bibliotheken en hulpmiddelen (zoals `golang.org/x/oauth2`, onder andere) om deze veiligere authenticatiemethoden te vergemakkelijken, waardoor het gemakkelijker wordt voor ontwikkelaars om veilige, effectieve en moderne autorisatiemechanismen in hun toepassingen te implementeren.
