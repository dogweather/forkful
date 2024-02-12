---
title:                "Kompleksilukujen käsittely"
aliases:
- /fi/elm/working-with-complex-numbers.md
date:                  2024-01-26T04:39:26.918818-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kompleksilukujen käsittely"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Kompleksiluvut ovat reaalilukujen ja imaginaarilukujen yhdistelmiä, kuten `a + bi`, missä `i` on -1:n neliöjuuri. Ne ovat avainasemassa aloilla kuten insinööritiede ja fysiikka ratkaisemaan ongelmia, joita tavallisilla luvuilla ei voi käsitellä.

## Kuinka:
Elm ei sisällä valmista tukea kompleksiluvuille, joten sinun täytyy luoda oma tyyppisi ja funktiosi. Tässä on nopea asennus:

```Elm
type alias Complex =
    { real : Float, imaginary : Float }

add : Complex -> Complex -> Complex
add a b =
    { real = a.real + b.real, imaginary = a.imaginary + b.imaginary }

-- Esimerkin käyttö:
a = { real = 3, imaginary = 2 }
b = { real = 1, imaginary = -4 }

summa = add a b
-- summa on { real = 4.0, imaginary = -2.0 }
```

## Syväsukellus
Historiallisesti kompleksilukuja ei aina ole hyväksytty. Ne muuttuivat pelinrakentajaksi 1500-luvulla ratkaisemaan kuutioyhtälöitä. Vaihtoehtoja muissa kielissä, kuten Python, tarjoavat valmiin tuen kompleksiluvuille operaatioineen suoraan laatikosta. Elm vaatii DIY-lähestymistavan, kuten olet nähnyt. Mutta voit tehdä siitä yhtä hienostuneen kuin tarvitset, rakentaen kertolaskun, jakolaskun ja muita operaatioita, säätäen suorituskykyongelmia.

## Katso myös
- Elmin virallinen dokumentaatio: https://package.elm-lang.org/ omien tyyppien luomiseen ja Elmin perusteiden hallitsemiseen.
- Matematiikan historian harrastajat voisivat tarkistaa "An Imaginary Tale" -kirjan Paul J. Nahinilta kompleksilukujen matkasta ajassa.
- Syvenny matematiikkaan suuntautuviin ohjelmointihaasteisiin Project Eulerissa (https://projecteuler.net) soveltaaksesi kompleksilukujen velhouttasi.
