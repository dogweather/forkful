---
title:    "Elm: Testien kirjoittaminen"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Miksi

Elm on suosittu ohjelmointikieli, joka tunnetaan vahvasti tyypitetyistä ja toiminnallisista ominaisuuksistaan. Yksi tärkeimmistä käytännöistä, jota kannattaa seurata Elm-ohjelmoinnissa, on testien kirjoittaminen. Testien kirjoittamisen avulla voimme varmistaa koodimme laadun ja välttää mahdollisia virheitä, mikä säästää aikaa ja vaivaa kehitystyössä. Seuraavassa kappaleessa käymme läpi, miten voit aloittaa testien kirjoittamisen Elm-projekteissasi.

## Miten

Aloittamiseksi voit kirjoittaa yksinkertaisen testin perusfunktiolle käyttämällä Elm-testikirjastoa. Tämän esimerkin avulla voit testata toimiiko funktio oikein ja tuottaako se odotetun tuloksen. 

```
Elm
describe "testiFunktio"
    [test "tulisi palauttaa oikea tulos" <|
        \() -> 
            testiFunktio 5 == 10
    ]
```

Tässä esimerkissä testi varmistaa, että funktio `testiFunktio` palauttaa odotetun tuloksen arvolla 10, kun parametrina annetaan luku 5. Tämän lisäksi voit myös testata toistoja ja virhekäsittelyä. Alamme nyt tutkia tarkemmin testien kirjoittamista Elm-projekteissa.

## Syvemmällä

Testien kirjoittamisessa Elm-projekteissa käytetään tyypillisesti Elm-testikirjastoa. Tämä kirjasto tarjoaa useita käteviä toimintoja kuten `test` ja `testExpect`. Näistä `test`-toiminto on yksinkertaisempi, sillä se vaatii vain testin kuvaavan nimen ja funktion, joka palauttaa Bool-arvon. `testExpect`-toiminto sen sijaan mahdollistaa tarkemman ododotetun arvon asettamisen testille.

Lisäksi voit käyttää `describe`-funktiota, joka mahdollistaa testien ryhmittelyn ja paremman organisoinnin. Voit myös käyttää `andThen`-toimintoa testien ketjuttamiseen. Tämä on hyödyllistä, kun haluat testata jotain sen jälkeen, kun muut testit on suoritettu.

Jos haluat syventää tietämystäsi testien kirjoittamisesta Elm-projekteissa, suosittelemme tutustumaan Elm-testikirjaston dokumentaatioon ja kokeilemaan erilaisia tapoja testien kirjoittamiseen.

## Katso myös

- [Elm-testikirjaston dokumentaatio](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- [Elm-testikirjaston esimerkit](https://github.com/elm-explorations/test/tree/master/examples)
- [Elm-testikirjaston käyttöönotto-opas](https://dev.to/swizardo/elm-test-c7c)