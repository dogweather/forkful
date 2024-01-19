---
title:                "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
html_title:           "Kotlin: Lähettäminen http-pyyntö perusautentikoinnin kanssa"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTTP-pyynnön lähettäminen perusautentikoinnilla on prosessi, jossa lähetetään pyyntö verkkoserverille kiinnittäen mukanaan käyttäjänimen ja salasanan tiedot. Ohjelmoijat käyttävät tätä tekniikkaa, kun heidän täytyy kommunikoida suojattujen web-resurssien kuten API-palveluiden kanssa.

## Miten se toimii:

```Go
package main

import (
	"net/http"
	"fmt"
)

func main() {
	req, err := http.NewRequest("GET", "https://api-url", nil)
	if err != nil {
		log.Fatalln(err)
	}
	req.SetBasicAuth("username", "password")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		log.Fatalln(err)
	}

	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	fmt.Println(string(body))
}
```

Tämä koodi luo uuden HTTP-pyynnön (GET), asettaa sille perusautentikointitiedot ja lähettää sen. Vaste tulostetaan.

## Syvällisempi tieto:

Perusautentikointi on ollut mukana HTTP-prokollan alkuvuosista saakka, mutta sen käyttö on vähentynyt sen yksinkertaisuuden ja turvatarkastusten puutteen vuoksi. Vaihtoehtoja ovat muun muassa kehittyneemmät autentikointimalleja, kuten OAuth ja JWT.

Perusautentikoinnin toteutuksessa Go:ssa, `SetBasicAuth` funktio asettaa `Authorization` otsikon arvoksi käyttäjänimen ja salasanan, jotka on koodattu base64:ään.

## Lisätietoja:

- [HTTP-autentikointi](https://developer.mozilla.org/fi/docs/Web/HTTP/Authentication)
- [Golangin http package dok](https://golang.org/pkg/net/http/)
- [Korvaavat autentikoinnit, kuten OAuth ja JWT](https://jwt.io/introduction/)