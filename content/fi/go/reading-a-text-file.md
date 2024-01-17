---
title:                "Tekstitiedoston lukeminen"
html_title:           "Go: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Tekstitiedoston lukeminen tarkoittaa, että ohjelma lukee tiedostossa olevan tekstin ja käsittelee sitä haluamallasi tavalla. Ohjelmoijat tekevät tätä esimerkiksi tietojen lukemiseksi tiedostoista tai käyttäjän syötteen käsittelyyn.

## Kuinka tehdä se:
Käyttämällä Go-ohjelmointikieltä, voit käyttää ```os.Open()``` -funktiota avataksesi tiedoston ja sitten ```bufio.Scanner``` -pakettia lukemaan tiedoston yksi rivi kerrallaan. Seuraavassa koodiesimerkissä avataan tiedosto nimeltä "tekstitiedosto.txt", luetaan yksi rivi ja tulostetaan se konsoliin.

```Go
tiedosto, err := os.Open("tekstitiedosto.txt")
if err != nil {
    fmt.Println("Virhe:", err)
    return
}
defer tiedosto.Close()

skanneri := bufio.NewScanner(tiedosto)
for skanneri.Scan() {
    fmt.Println(skanneri.Text())
}
```

Tuloste:

```
Tämä on ensimmäinen rivi.
Tämä on toinen rivi.
Tervetuloa Go-ohjelmointiin!
```

## Syväsukellus:
Tiedostojen lukeminen on yksi tärkeimmistä toiminnoista ohjelmoinnissa, ja sitä tarvitaan usein tietojen käsittelyssä. Tekstivirtojen lukeminen on ollut osa ohjelmointia jo pitkään, ja Go tarjoaa tehokkaan tavan tehdä se helposti ja yksinkertaisesti.

On myös muita tapoja lukea tekstitiedostoja Go-kielellä, kuten ```ioutil.ReadFile()``` -funktiolla, mutta ```bufio.Scanner``` tarjoaa enemmän joustavuutta ja paremman suorituskyvyn tiettyjen olosuhteiden alaisena.

Jos haluat syvällisempää tietoa lukemisesta tiedostoihin, voit tutustua Go:n dokumentaatioon ja katsella muita esimerkkejä.

## Katso myös:
- [bufio.Scanner dokumentaatio](https://golang.org/pkg/bufio/#Scanner)
- [Go:n virallinen dokumentaatio](https://golang.org/doc/)