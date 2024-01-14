---
title:                "Javascript: Tulostuksen virheenkorjaustulosteet"
programming_language: "Javascript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

# Miksi ohjelmoijien tulisi käyttää debug-tulostusta?

Debug-tulostus on tärkeä työkalu ohjelmoijille, sillä se auttaa selvittämään ohjelmien virheitä ja ongelmia. Kun ohjelma ei toimi odotetulla tavalla, debug-tulostus voi antaa tärkeää tietoa siitä, mistä ongelma johtuu ja auttaa korjaamaan sen.

## Miten käyttää debug-tulostusta

Debug-tulostuksen käyttö on melko yksinkertaista. Ensinnäkin, sinun tulee sisällyttää koodiisi print-komento, joka tulostaa haluamasi tiedot. Esimerkiksi:

```Javascript
console.log("Tervetuloa debuggaamaan ohjelmaa!");
```

Tämä tulostaa konsoliin "Tervetuloa debuggaamaan ohjelmaa!".

Voit myös tulostaa muuttujien arvoja debug-tulostuksen avulla, mikä voi auttaa hahmottamaan ohjelman toimintaa. Esimerkiksi:

```Javascript
let nimi = "Maija";
console.log("Hei " + nimi + ", tervetuloa sivustolle!");
```

Tämä tulostaisi "Hei Maija, tervetuloa sivustolle!".

## Syventävä tietoa debug-tulostuksesta

Debug-tulostuksen käyttö voi olla hyödyllistä myös silloin, kun sinulla on monimutkaisempi koodi, jota on vaikea hahmottaa ohjelman toimiessa. Voit tulostaa debug-tulostuksia vaiheittain koodin suorituksen aikana saadaksesi paremman kuvan siitä, miten ohjelma toimii.

Debug-tulostuksen avulla voit myös selvittää, missä kohtaa koodia virhe tapahtuu ja mitkä muuttujat vaikuttavat siihen. Tämä voi säästää paljon aikaa virheiden etsinnässä ja korjaamisessa.

Lopuksi, on tärkeää muistaa poistaa debug-tulostukset lopullisesta koodista, sillä ne voivat hidastaa ohjelman suoritusta ja tehdä koodista sekavan.

## Katso myös

- [Debuggausopas Javascriptille](https://www.w3schools.com/js/js_debugging.asp)
- [Debug-tulostuksen käyttö Node.js:ssä](https://nodejs.org/de/docs/guides/debugging-getting-started/)
- [Vianmäärityksen perusteet ohjelmoinnissa](https://www.codingdojo.com/blog/20-pro-tips-and-tools-for-debugging-any-code)