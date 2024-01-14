---
title:                "Gleam: Uuden projektin aloittaminen"
simple_title:         "Uuden projektin aloittaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi
Monesti aloittaminen uuden projektin kanssa voi tuntua haastavalta ja pelottavalta, mutta Gleam-ohjelmointikielen avulla aloittaminen on helppoa ja palkitsevaa. Gleam tarjoaa tehokkaan ja helppokäyttöisen tavan rakentaa funktionaalisia ohjelmia ja monenlaisia sovelluksia, joten sen oppiminen on erittäin hyödyllistä ja tulevaisuuden kannalta tärkeää.

## Kuinka
Aloittaminen Gleam-ohjelmoinnilla on yksinkertaista ja se vaatii vain muutamia vaiheita. Voit aloittaa uuden projektisi luomalla uuden Gleam-tiedoston ja kirjoittamalla koodia kuten seuraava esimerkki:
```Gleam
fn laske_juoma_hinta(hinta, alv) {
  hinta + (hinta * alv)
}

let juoman_hinta = 10.00
let alv = 0.24
let kokonaishinta = laske_juoma_hinta(juoman_hinta, alv)

io.format("Juoman kokonaishinta on $.", [kokonaishinta])
```
Tässä esimerkissä luodaan funktio, joka laskee juoman kokonaishinnan alv:n kanssa. Tämän jälkeen muuttujat asetetaan ja funktiota kutsutaan. Tuloksena saadaan muotoiltu tulostus, joka näyttää juoman kokonaishinnan.

## Syväsyvennyksellä
Gleam-ohjelmoinnin aloittaminen ei vaadi suurta ponnistelua, mutta jos haluat perehtyä syvemmin sen ominaisuuksiin, se on myös mahdollista. Voit tutustua kattavaan Gleam-dokumentaatioon, josta löydät tietoa muun muassa kielestä, sen rakenteista ja käytönideoista. Voit myös liittyä Gleam-yhteisön Discord-kanavalle, jossa voit keskustella muiden ohjelmoijien kanssa, jakaa ideoita ja oppia uusia asioita. Aloita tutustuminen Gleam-ohjelmointiin jo tänään ja löydät pian itsesi luomasta upeita ja tehokkaita ohjelmia.

## Katso myös
- [Gleam-dokumentaatio](https://gleam.run/documentation/)
- [Gleam-yhteisö Discord](https://discord.gg/gleam-lang)
- [Blogi: "Mitä hyötyä Gleam-ohjelmoinnista voi olla"?](https://medium.com/@gleamlang/what-can-gleam-programming-do-for-you-b6a6c6834d6c)