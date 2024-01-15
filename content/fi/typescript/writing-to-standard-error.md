---
title:                "Kirjoittaminen standardivirheeseen"
html_title:           "TypeScript: Kirjoittaminen standardivirheeseen"
simple_title:         "Kirjoittaminen standardivirheeseen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen standardivirheelle on tärkeä taito jokaiselle TypeScript-ohjelmoijalle. Se mahdollistaa virheiden tunnistamisen ja korjaamisen ohjelmoinnin aikana, mikä voi säästää paljon aikaa ja vaivaa virheiden etsimiseltä jälkikäteen.

## Mitä ja Miten

Kirjoittaminen standardivirheelle tapahtuu käyttämällä `console.error()` -funktiota, joka ottaa parametrinaan virheilmoituksen tai muun halutun tekstin. Tämä funktio tulostaa tekstin konsolille punaisella värillä, mikä helpottaa sen erottamista muusta koodista.

```TypeScript
console.error("Tämä on virheilmoitus.");
```

Tämän esimerkin tulostus näyttäisi tältä:

```
Tämä on virheilmoitus.
```

Voit myös käyttää muuttujia ja string templating -ominaisuutta lisätäksesi tarkempaa tietoa virheestä. Esimerkiksi:

```TypeScript
const virheKoodi = 404;
console.error(`Sivua ei löytynyt. Virhe: ${virheKoodi}`);
```

Tämän tulostus näyttäisi tältä:

```
Sivua ei löytynyt. Virhe: 404
```

## Syvempi Sukellus

Kirjoittaminen standardivirheelle on erityisen hyödyllistä silloin, kun haluat tarkkailla tiettyjä osia koodistasi tai saada lisätietoa tapahtuneesta virheestä. Voit myös käyttää `console.trace()` -funktiota, joka tulostaa koko koodin suoritusjonon, mikä auttaa hahmottamaan, missä kohtaa virhe on tapahtunut.

Toinen hyödyllinen tapa käyttää `console.error()` on luoda omia virheilmoituksia ja kustomoituja virhekoodit, jotka auttavat tunnistamaan tietyn tyyppisiä virheitä ohjelmassa. Esimerkiksi:

```TypeScript
const virheKoodit = {
  404: "Sivua ei löytynyt.",
  500: "Palvelinvirhe."
}

console.error(virheKoodit[404]);
```

Tämän tulostus näyttäisi tältä:

```
Sivua ei löytynyt.
```

## Katso myös

- [TypeScriptin virallinen dokumentaatio](https://www.typescriptlang.org/docs/home.html)
- [Kirjoittaminen standardivirheelle MDN:ssä](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)