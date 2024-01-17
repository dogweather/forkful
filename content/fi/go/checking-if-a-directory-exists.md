---
title:                "Tarkistetaan, onko hakemistoa olemassa"
html_title:           "Go: Tarkistetaan, onko hakemistoa olemassa"
simple_title:         "Tarkistetaan, onko hakemistoa olemassa"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

Mitä ja miksi?
Tarkistaminen, onko hakemisto olemassa, on tapa tarkistaa, onko tietyllä polulla oleva hakemisto olemassa vai ei. Tämä on tärkeää monissa ohjelmoinnin tilanteissa, kuten varmistaessa, että tarvittavat tiedostot löytyvät ennen niiden käsittelyä.

Miten:
Voit tarkistaa, onko hakemisto olemassa Go-kielen osana tarjottavilla toiminnoilla. Esimerkiksi ```os.Stat()``` -toiminto palauttaa virheen, jos tiedostoa ei löydy annetusta polusta. Alla oleva koodiesimerkki näyttää, miten tarkistaa, onko "hakemisto" niminen hakemisto olemassa ja tulostaa sen olemassaolon mukaisen viestin.

```Go
func main() {
    if _, err := os.Stat("hakemisto"); os.IsNotExist(err) {
        fmt.Println("Hakemistoa ei löydy")
    } else {
        fmt.Println("Hakemisto löytyy")
    }
}
```

Esimerkkilähtö:

```
Hakemistoa ei löydy
```

Syvä päätyminen:
Hakemistojen olemassaolon tarkistaminen on ollut osa monia ohjelmointikieliä jo pitkään. Yleinen tapa tarkistaa olemassaolo on kokeilla pääsyä hakemistoon ja käsitellä havaitut virheet sen perusteella. Lisäksi voit käyttää myös muita toimintoja, kuten ```os.Open()```, joka palauttaa virheen, jos hakemistoa ei löydy. Lopuksi, jotkut käyttävät myös ```os.Lstat()``` -toimintoa, joka toimii samoin kuin ```os.Stat()```, mutta palauttaa myös tiedoston tai hakemiston tiedot.

Katso myös:
- Go-kirjasto pääsy tiedostoihin ja hakemistoihin: https://golang.org/pkg/os/
- Go-kurssi, joka käsittelee tiedostojen ja hakemistojen käsittelyä: https://tour.golang.org/programs/12
- "Tarkista hakemiston olemassaolo Go-kielellä" -artikkeli: https://www.digitalocean.com/community/tutorials/how-to-check-if-a-directory-exists-in-go-fi