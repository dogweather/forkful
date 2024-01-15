---
title:                "Nykyisen päivämäärän saaminen"
html_title:           "TypeScript: Nykyisen päivämäärän saaminen"
simple_title:         "Nykyisen päivämäärän saaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi joku voisi haluta saada nykyinen päivämäärä ohjelmassaan. Joissakin tapauksissa se voi auttaa tekemään päätöksiä tai laskelmia, kun tunnetaan tämänhetkinen päivämäärä. Toisinaan se voi myös olla osa suurempaa toimintoa, kuten kirjanpitoa tai aikaleimaa.

## Miten tehdä se

Voit helposti saada nykyisen päivämäärän TypeScriptissä käyttäen Date-objektia ja sen sisäänrakennettuja metodeja. Voit aloittaa luomalla uuden Date-objektin ja tallentamalla sen muuttujaan.

```TypeScript
let currentDate = new Date();
```

Tämän jälkeen voit käyttää erilaisia Date-objektin metodeja saadaksesi haluamasi päivämäärän tiedot. Näitä ovat esimerkiksi getFullYear(), getMonth(), getDate(), getDay(), jne.

```TypeScript
let year = currentDate.getFullYear();
let month = currentDate.getMonth();
let day = currentDate.getDate();
let weekday = currentDate.getDay();
```

Voit myös muotoilla päivämäärän haluamallasi tavalla käyttämällä Date-objektin toMethod-metodia ja erilaisia formaattimerkkejä.

```TypeScript
let formattedDate = currentDate.toDateMethod("dd/mm/yyyy");
console.log(formattedDate); // 04/09/2021
```

## Syväsukellus

Vaikka JavaScript ja siten myös TypeScript käyttävät Date-objektia päivämäärän ja ajan hallintaan, se käyttää myös Unix Epoch -aikaleimaa, joka tarkoittaa aikaa, joka on kulunut 1. tammikuuta 1970 klo 00:00:00 UTC:sta. Päivämäärä ja aika tallennetaan millisekunneissa tästä ajankohdasta lähtien. Tätä aikaleimaa voidaan käyttää muuntamaan eri ajanjaksoja millisekunneiksi nykyhetkestä, ja päinvastoin.

Date-objektin metodeilla on myös muita hyödyllisiä toimintoja, kuten asettaa päivämäärän tai ajan arvoja, muuntaa aikavyöhykkeitä ja tarkistaa päivämäärän validius.

## Katso myös

- [Date-objekti TypeScriptin virallisessa dokumentaatiossa](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-0.html)
- [Unix Epoch-aikaleima Wikipediassa](https://en.wikipedia.org/wiki/Unix_time)