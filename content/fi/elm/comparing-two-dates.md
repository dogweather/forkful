---
title:                "Vertailemalla kahta päivämäärää"
html_title:           "Elm: Vertailemalla kahta päivämäärää"
simple_title:         "Vertailemalla kahta päivämäärää"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

On olemassa monia tilanteita, joissa joudumme vertailemaan kahta päivämäärää. Saatamme esimerkiksi tarvita tätä tietoa ohjelmassamme, joka laskee päivien eroja, tai vaikkapa käyttäessämme verkkokauppaa ja haluamme nähdä, milloin tilauksemme on toimitettu.

## Kuinka

```Elm
import Date

--Määritellään kaksi päivämäärää
date1 = Date.fromString "2021-08-02"
date2 = Date.fromString "2021-08-10"

--Vertaillaan päivämääriä funktiolla Date.compare
--Tulos on Ordering-tyyppiä, joka voi olla LT (päivämäärä1 on ennen päivämäärää2), GT (päivämäärä1 on jälkeen päivämäärää2) tai EQ (päivämäärät ovat samat)
comparison = Date.compare date1 date2

--Tulostetaan vertailun tulos
case comparison of 
    LT ->
        "Päivämäärä1 on ennen Päivämäärää2"
    GT ->
        "Päivämäärä1 on jälkeen Päivämäärää2"
    EQ ->
        "Päivämäärät ovat samat"
``` 
Tässä esimerkissä tuodaan ensin käytettäväksi Date-kirjasto ja määritellään kaksi päivämäärää merkkijonoina. Sitten käytetään funktiota Date.compare, joka vertailee annettuja päivämääriä ja palauttaa tuloksen, joka tallennetaan muuttujaan. Lopuksi tulostetaan sopiva viesti vertailun tuloksesta käyttämällä koodin haaraumia.

## Syvempi sukellus

Elmillä on sisäänrakennettu Date-kirjasto, joka tarjoaa erilaisia toimintoja päivämäärien käsittelyyn. Esimerkissä käytettiin funktiota Date.fromString, joka muuttaa merkkijonon päivämääräksi. Lisäksi Date-kirjastosta löytyy myös muita hyödyllisiä funktioita, kuten addDays ja subDays, joilla voi lisätä tai vähentää päiviä annetusta päivämäärästä.

On myös tärkeää huomata, että Date.compare-funktio vertailee päivämääriä aikamuodossa, jossa vuosiluvut kasvavat suuremmiksi. Jos siis haluat vertailla päivämääriä esimerkiksi historiallisessa kontekstissa, kannattaa ensin muuttaa ne aikatempeliksi.

## Katso myös

- [Elmin virallinen dokumentaatio Date-kirjastosta](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Learn Elm: Working with Dates](https://www.learningsomethingnew.com/learn-elm-working-with-dates/)