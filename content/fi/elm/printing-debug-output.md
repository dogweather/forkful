---
title:                "Virheenkorjaustulosteen tulostaminen"
html_title:           "Elm: Virheenkorjaustulosteen tulostaminen"
simple_title:         "Virheenkorjaustulosteen tulostaminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Tulostusdebuggaus on ohjelmoijien tapa tarkistaa koodin toimivuus ja tunnistaa mahdollisia virheitä. Tämän avulla voidaan seurata koodin suoritusta ja selvittää, mitä tapahtuu kunkin vaiheen aikana.

## Miten:
```Elm
-- Esimerkki tulostusdebuggauksesta
main = 
    let
        num1 = 5
        num2 = 7
        tulos = num1 + num2
    in
        Debug.log "Tulos" tulos
```

Tämä koodinpätkä tulostaa konsoliin tekstin "Tulos" ja sen perään laskun tulos, joka tässä tapauksessa on 12. Näin voidaan varmistaa, että laskutoimitus suoritetaan oikein ja tarvittaessa korjata mahdollisia virheitä.

## Syväsukellus:
Tulostusdebuggaus on ollut käytössä ohjelmoinnissa jo pitkään ja se perustuu yksinkertaiseen ideaan: koodin suorituksen seurantaan. Sen lisäksi, että se auttaa tunnistamaan virheitä, se voi myös auttaa ymmärtämään koodin toimintaa paremmin ja nopeuttamaan kehitystyötä.

Vaihtoehtoisena tapana on käyttää testejä, jotka testaavat koodin erilaisia toimintoja. Tämä voi olla hyödyllistä, mutta printtausdebuggauksen avulla saadaan parempi käsitys koodin toiminnasta ja mahdollisista ongelmakohdista.

Tulostusdebuggauksen toteuttaminen tapahtuu käyttämällä ```Debug``` moduulia, joka tarjoaa erilaisia toimintoja tulostamiseen, esimerkiksi ```log``` ja ```crash```.


## Katso myös:
- [Elm-tiimin opas tulostusdebuggaukseen](https://guide.elm-lang.org/debugging/)
- [Debug-moduulin dokumentaatio](https://package.elm-lang.org/packages/elm/core/latest/Debug)