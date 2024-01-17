---
title:                "Tulostaminen virheenkorjaustulosteen"
html_title:           "Swift: Tulostaminen virheenkorjaustulosteen"
simple_title:         "Tulostaminen virheenkorjaustulosteen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Debug-tulostus tarkoittaa ohjelman tulostamista koodin suorituksen eri vaiheissa havainnollistamaan sen toimintaa. Tämä on erityisen hyödyllistä havaittaessa virheitä tai epäselvyyksiä koodissa ja etsiessä niiden syitä. 

## Miten:

```Swift
print("Hei Maailma!")
```
Käyttäessäsi `print`-komentoa, voit tulostaa haluamasi arvon konsoliin. Tämä on yksi yksinkertaisimmista tavoista tarkistaa, onko koodisi toiminnassa odotetulla tavalla. Voit myös käyttää `dump`-komentoa tulostamaan tarkemman tiedon kompleksisemmista arvoista. 

## Syvempi sukellus:

Debug-tulostus on ollut osa ohjelmointikulttuuria jo pitkään, ja sen avulla on helpotettu koodin virheiden etsimistä ja korjaamista. Nykyään vaihtoehtoina debug-tulostukselle ovat myös esimerkiksi koodin tarkkailu ja virheenkorjausohjelmat. 

On myös tärkeää muistaa poistaa debug-tulostukset lopullisesta koodista ennen sen julkaisemista, jotta ohjelman suoritus ei hidastu tarpeettomasti. Debug-tulostus voidaan myös ottaa käyttöön tai poistaa käytöstä riippuen siitä, haluatko tarkistaa koodin toimintaa vai et.

## Katso myös:

[Lisää tietoa print-komennosta](https://developer.apple.com/documentation/swift/1541053-print)