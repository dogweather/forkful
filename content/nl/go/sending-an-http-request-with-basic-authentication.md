---
title:                "Een HTTP-verzoek verzenden met basisauthenticatie"
date:                  2024-01-28T22:07:44.578547-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een HTTP-verzoek verzenden met basisauthenticatie"

category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
HTTP-verzoeken met basisauthenticatie voegen een eenvoudige beveiligingslaag toe aan een API-aanroep. Programmeurs gebruiken het om toegang te krijgen tot bronnen die inloggegevens vereisen, zoals gebruikersspecifieke gegevens.

## Hoe te:
Het verzenden van een geauthenticeerd HTTP-verzoek is eenvoudig in Go:

```Go
package main

import (
	"encoding/base64"
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	client := &http.Client{}
	req, err := http.NewRequest("GET", "https://api.example.com/data", nil)
	if err != nil {
		panic(err)
	}

	username := "user"
	password := "pass"
	credentials := base64.StdEncoding.EncodeToString([]byte(username + ":" + password))
	req.Header.Add("Authorization", "Basic "+credentials)

	resp, err := client.Do(req)
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		panic(err)
	}
	fmt.Printf("%s\n", body)
}
```

Voorbeelduitvoer (met fictieve API-URL en inloggegevens):
```plaintext
{"status":"success","data":"some private data"}
```

## Diepere duik
Basisauthenticatie is onderdeel van de HTTP/1.0-specificatie en bestaat al sinds de vroege dagen van het web. Het is niet de meest veilige methode (inloggegevens zijn slechts base64-gecodeerd, niet versleuteld), dus wordt het vaak vervangen door OAuth of JWT in meer gevoelige toepassingen.

Wat implementatie betreft, bevat Go ingebouwde ondersteuning voor HTTP-clients en -verzoeken, met het pakket `net/http` waarmee ontwikkelaars webverkeer kunnen afhandelen. Wanneer we basisauthenticatie gebruiken, moeten we ervoor zorgen dat de inloggegevens correct zijn gecodeerd en dat de `Authorization`-header wordt toegevoegd aan het HTTP-verzoek.

Hoewel eenvoudig, moet je basisauthenticatie via gewoon HTTP vermijden omdat het gevoelig is voor man-in-the-middle-aanvallen. Gebruik altijd HTTPS wanneer je inloggegevens verstuurt.

## Zie ook
- Go `net/http`-pakketdocumentatie: https://pkg.go.dev/net/http
- Go `encoding/base64`-pakketdocumentatie: https://pkg.go.dev/encoding/base64
- Info over HTTP-basisauthenticatie: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Voor veiligere authenticatiemethoden: https://oauth.net/ en https://jwt.io/
