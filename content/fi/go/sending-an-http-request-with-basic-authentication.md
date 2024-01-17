---
title:                "Lähettämällä http-pyyntö perusautentikoinnilla"
html_title:           "Go: Lähettämällä http-pyyntö perusautentikoinnilla"
simple_title:         "Lähettämällä http-pyyntö perusautentikoinnilla"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Mitä & Miksi?: Lähettäminen HTTP-pyyntö perusautentikoinnilla on tapa tarkistaa käyttäjän tunnistetiedot ennen pääsyä tiettyihin verkkopalveluihin. Tämä on tärkeää, jotta estäisi luvattomat käyttäjät pääsemästä pääsyä tietojärjestelmiin.

Miten: Alla on esimerkki koodista Go-kielellä, jossa lähetetään HTTP-pyyntö perusautentikoinnilla ja tulostetaan vastauksen statuskoodi sekä sisältö:

'''
package main

import (
	"fmt"
	"net/http"
	"io/ioutil"
)

func main() {
	// Määritellään pyyntö
	req, _ := http.NewRequest("GET", "https://example.com", nil)

	// Lisätään autentikointi headeriin
	req.SetBasicAuth("käyttäjänimi", "salasana")

	// Lähetetään pyyntö
	resp, _ := http.DefaultClient.Do(req)

	// Tulostetaan vastauksen statuskoodi
	fmt.Println("Status: ", resp.Status)

	// Luetaan vastauksen sisältö
	body, _ := ioutil.ReadAll(resp.Body)

	// Tulostetaan sisältö
	fmt.Println("Sisältö: ", string(body))
}
'''

Tulostaa:

Status: 200 OK
Sisältö: <h1>Tervetuloa</h1>

Deep Dive: Perusautentikointi on yksi vanhimmista tavoista tunnistautua verkkopalveluun ja se on yhä tärkeä osa monien verkkosovellusten turvallisuutta. On myös muita tapoja lähettää HTTP-pyyntöjä, kuten määrittää API-avain tai OAuth-tunnistus, mutta perusautentikointi on yhä käytössä esimerkiksi sisäisten järjestelmien välisessä kommunikaatiossa.

Katso myös: Jos haluat oppia lisää perusautentikoinnista ja sen toteutuksesta Go-kielellä, niin suosittelemme lukemaan Go-kirjaston "net/http" dokumentaatiota ja kokeilemaan erilaisia lähestymistapoja lähettää HTTP-pyyntö perusautentikoinnilla. Voit myös tutustua muihin HTTP-tunnistusmenetelmiin, kuten Digest-autentikointiin, joka tarjoaa paremman salauksen salasanoille.