---
title:                "Tulostaminen virheenjäljitystiedostoon"
html_title:           "Swift: Tulostaminen virheenjäljitystiedostoon"
simple_title:         "Tulostaminen virheenjäljitystiedostoon"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Debug-tulosteen tulostaminen voi olla tärkeää ohjelmoinnin aikana, kun yrität löytää virheitä tai hahmottaa ohjelman toimintaa. Se voi auttaa sinua näkemään, mitä tietoja ohjelma käsittelee ja mikä voi mennä vikaan.

## Kuinka tehdä

Yksinkertaisin tapa tulostaa debug-tulosteita Swiftissä on käyttää "print" -funktiota. Tämä funktio ottaa vastaan ​​yksittäisen parametrin ja tulostaa sen konsoliin.

```Swift
print("Tämä on debug-tuloste")
```

Voit myös tulostaa useampia arvoja samalla kertaa käyttämällä pilkulla erotettua listausta:

```Swift
let numero = 5
let sana = "Hei"
print(numero, sana) // Tulostaa "5 Hei"
```

Jos haluat tulostaa monimutkaisemman rakenteen, kuten taulukon tai sanakirjan, voit käyttää ```dump``` -funktiota. Tämä tulostaa kaikki rakenteen tiedot, jotta voit tarkastella niitä tarkemmin.

```Swift
let lista = ["omena", "banaani", "appelsiini"]
dump(lista) // Tulostaa "[3 elements]"
```

Voit myös ohjata debug-tulosteet eri lähteisiin käyttämällä "print" -funktion erilaisia ​​muotoja. Esimerkiksi voit tulostaa tiedostoon tai lähiverkkoon.

## Syväsukellus

Yllä mainitut esimerkit koskivat yksinkertaisia ​​tulosteita, mutta usein tarvitset enemmän tietoa debug-tulosteella. Voit lisätä lisävarmuuksia tulosteisiin käyttämällä ehtolausekkeita tai toistolausekkeita.

```Swift
let luku = 10

if luku > 5 {
    print("Luku on suurempi kuin 5")
}

for i in 1...5 {
    print("Toistetaan \(i). kerta")
}
```

Voit myös käyttää muotoilumerkintöjä tulosteissa, jotta voit lisätä muuttujien arvoja suoraan tulosteeseen.

```Swift
let nimi = "Matti"
let ika = 33

print("Hei, minun nimeni on \(nimi) ja olen \(ika) vuotta vanha")
```

Lopuksi, voit asettaa omia tulostusasetuksia, kuten erilaisen erottimen tai uuden rivin lisäämisen tulosteen loppuun.

## Katso myös

- [Swiftin debuggausvinkit ja -temput](https://www.hackingwithswift.com/debugging)
- [SWiftin debug-tulosteiden hallinta](https://dev.to/aquilesb/how-to-manage-swift-debug-output-4a4n)