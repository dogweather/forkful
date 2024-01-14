---
title:                "Go: Tekstin haku ja korvaaminen"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Suurimmaksi osaksi ohjelmoijien työ koostuu koodin kirjoittamisesta ja sen jatkuvaan muokkaamista. On monia tilanteita, jossa ohjelmakoodin sisältämä teksti kaipaa vaihtamista tai korvaamista toisella tekstillä. Tämä voi olla aikaa vievää ja työlästä, mutta Go-ohjelmointikielessä on onneksi helppo ja tehokas tapa tehdä tätä: tekstien etsiminen ja korvaaminen. 

## Miten tehdä

Go-koodissa tekstien etsiminen ja korvaaminen voidaan tehdä käyttämällä "strings" -kirjastoa. Kirjasto tarjoaa monia toimintoja, joita voit hyödyntää erilaisten tekstinmuokkaustehtävien suorittamiseen.

Esimerkiksi, voit käyttää "ReplaceAll" -toimintoa vaihtamalla tekstiä luomiasi muuttujia ja tulostaa uuden tekstin "fmt.Println" -toiminnolla. Katso alla oleva esimerkki:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	teksti := "Tervetuloa, maailma!"
	uusiTeksti := strings.ReplaceAll(teksti, "maailma", "Go-koodaajat")
	fmt.Println(uusiTeksti)
}
```

Tulostus:

```Go
Tervetuloa, Go-koodaajat!
```

## Syvällisempi sukellus

Go-ohjelmointikielen "strings" -kirjasto tarjoaa myös muita hyödyllisiä toimintoja kuten "LastIndex" ja "Repeat". Näitä toimintoja voidaan käyttää monimutkaisempiin tekstinmuokkaustehtäviin, kuten tietyn merkin etsimiseen täysin uudesta tekstistä tai tietyn tekstin toistamiseen useita kertoja.

On myös tärkeää huomata, että "strings" -kirjasto tarjoaa myös toimintoja, jotka toimivat osittaisten merkkijonojen kanssa, kuten "HasPrefix" ja "HasSuffix". Nämä toiminnot ovat erittäin hyödyllisiä tekstin etsimisessä ja korvaamisessa tapauksissa, joissa haluat muokata vain osaa tekstistä.

## Katso myös

- [Go-ohjelmointikielen virallinen sivusto](https://golang.org/)
- [Go-ohjelmointikielen dokumentaatio](https://golang.org/doc/)
- [Go-ohjelmointikielen "strings" -kirjaston dokumentaatio](https://golang.org/pkg/strings/)