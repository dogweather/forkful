---
title:    "Go: Kirjoittaminen standardivirheelle"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi kirjoittaa standardiin virheenkorjaukseen
Kirjoittaminen standardiin virheenkorjaukseen on välttämätöntä ohjelmoijalle, joka haluaa selvittää ohjelmassaan mahdollisesti ilmeneviä virheitä. Näin virheilmoitukset tulevat näkyviin ohjelman suorituksen aikana ja virheiden etsiminen ja korjaaminen on helpompaa.

## Kuinka kirjoittaa standardiin virheenkorjaukseen
```Go
package main

import "fmt"
import "os"

func main() {
    _, err := os.Open("ei_olemassaolevaa_tiedostoa.txt")
    if err != nil {
        fmt.Fprintln(os.Stderr, "Virhe: tiedostoa ei löydy")
    }
}
```
Tässä esimerkissä käytetään Go-kielen sisäänrakennettua "fmt" ja "os" kirjastoa virheen tulostamiseen "stderr" (standard error) -virtaan. Tämä tulostaa virheen ilmoituksen ohjelman suorituksen aikana, jolloin ohjelmointivirheet on helppo löytää ja korjata.

## Syvempää tietoa standardiin virheenkorjaukseen
Yleensä ohjelmoija käyttää "fmt" kirjastoa virheen tulostamiseen standardiin virheenkorjausvirtaan, mutta myös "log" ja "errors" kirjastoja voidaan käyttää samaan tarkoitukseen. Nämä tarjoavat enemmän ominaisuuksia, kuten eri merkityksillä varustettuja virheitä ja virhesanomien muotoilua. On tärkeää muistaa, että virheen tulostaminen ja käsittely tulisi tehdä huolellisesti ja hyväksyttävällä tavalla ohjelman suorituksen aikana.

## Katso myös
- [Go virheen käsittely](https://gobyexample.com/errors)
- [Go "fmt" kirjasto](https://golang.org/pkg/fmt/)
- [Go "log" kirjasto](https://golang.org/pkg/log/)
- [Go "errors" kirjasto](https://golang.org/pkg/errors/)