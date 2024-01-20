---
title:                "Kahden päivämäärän vertaaminen"
html_title:           "Bash: Kahden päivämäärän vertaaminen"
simple_title:         "Kahden päivämäärän vertaaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Vertailemme kahta päivämäärää ymmärtääksemme, kumpi tulee ensin tai kuinka paljon aikaa on kulunut niiden välillä. Tämä on tärkeää, kun tehtäviä, tapahtumia tai toimintoja täytyy järjestää tai ajoittaa.

## Miten Tehdään:

```TypeScript
let date1 = new Date("2021-07-01");
let date2 = new Date("2021-07-02");

if (date1 < date2){
  console.log("Date1 tulee ennen Date2:tta");
} else if (date1 > date2) {
  console.log("Date1 tulee Date2:n jälkeen");
} else {
  console.log("Date1 ja Date2 ovat samat");
}
```
Näyte tuotos:
```
Date1 tulee ennen Date2:tta
```

## Syvällisempi Tarkastelu:

Päivämäärien vertailu ei ole aina niin yksinkertaista, koska se riippuu suuresti aikavyöhykkeestä, vuorokaudesta tai jopa kesäajasta. JavaScriptin Date-objektin ja TypeScriptin Date-tyypin avulla voidaan kuitenkin vertailla päivämääriä tehokkaasti.

Vaihtoehtoisia menetelmiä päivämäärän vertailuun voivat olla kirjastot, kuten Moment.js tai date-fns. Nämä kirjastot tarjoavat useita funktioita ja menetelmiä päivämäärän vertailuun ja käsittelyyn.

TypeScriptissä päivämäärien vertailu toteutetaan muuntamalla ne ensin millisekunneiksi kutsulla getTime(). Tämä palauttaa päivämäärän ja ajan millisekunneiksi 1. tammikuuta 1970 (UTC) jälkeen, ja se on vertailukelpoinen.

## Katso myös:

- [JavaScript Date-objekti - MDN Web Docs](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [date-fns](https://date-fns.org/)