---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
html_title:           "Go: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Miksi ja miten ohjelmoijat muuttavat merkkijonon pieniksi kirjaimiksi? Jokainen merkkijono sisältää sekä isoja että pieniä kirjaimia, ja joskus haluamme muuttaa ne tiettyyn muotoon. Esimerkiksi saman merkkijonon vertaaminen olisi helpompaa, jos kaikki kirjaimet ovat samassa koossa. Siksi ohjelmoijat käyttävät usein toimintoa, joka muuntaa merkkijonon kaikki kirjaimet pieniksi.

## Miten: 

```Go
package main 
import "strings" 
import "fmt" 
func main() { 
  fmt.Println(strings.ToLower("TEKSTI MERKKIJONO")) 
} 
```

Tuloste: teksti merkkijono 

Toimiakseen funktion täytyy kutsua "Strings" pakkaus Go-kielellä ja käyttää "ToLower" funktiota. Tämä muuttaa merkkijonon kaikki kirjaimet pieniksi. Lopuksi tulostamme uuden muokatun merkkijonon.

## Syvällinen sukellus:

Go-kielen "strings" -pakkauksen lisäksi on myös muita vaihtoehtoja, kuten "unicode" ja "regexp" -paketit, joita voidaan käyttää merkkijonon muuntamiseen pieniksi kirjaimiksi. Jotkut kielet, kuten Python, tarjoavat myös sisäänrakennetun toiminnon tähän tarkoitukseen.

Merkkijonon muuntaminen pieniksi kirjaimiksi ei myöskään ole uusi konsepti. Se on ollut olemassa ohjelmoinnissa pitkään, ja sen alkuperäiset käyttötarkoitukset liittyvät tekstin analysointiin ja käsittelyyn.

## Katso myös:

Lisätietoja "strings" pakkauksesta ja sen tarjoamista funktioista löydät alla olevasta linkistä: 
https://golang.org/pkg/strings/

Toinen hyödyllinen lähde merkkijonon muuntamisesta pieniksi kirjaimiksi on Go-opetusohjelma nimeltä "A Tour of Go": 
https://tour.golang.org/basics/11