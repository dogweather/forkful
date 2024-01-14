---
title:    "TypeScript: Komentoriviparametrien lukeminen"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Komentorivin argumenttien lukeminen on tärkeä taito TypeScript-ohjelmoijille. Tämä mahdollistaa käyttäjän antamien parametrien hyödyntämisen ohjelmassa ja tekee siitä interaktiivisemman ja joustavamman.

## Miten tehdä se

Komentorivin argumenttien lukeminen TypeScriptilla on helppoa! Sinun tarvitsee vain seurata näitä yksinkertaisia vaiheita:

1. Luo uusi TypeScript-tiedosto
2. Lisää komentorivin argumenttien lukemisesta vastaava kirjasto npm:n kautta `import * as arg from 'arg';`
3. Luodaan argumenttiluettelo määrittelemällä objekti, jonka avaimet ovat argumentit ja arvot ovat kuvaus niistä. Esimerkiksi: `const args = arg({ '--nimi': String, '--ika': Number });`
4. Voit sitten käyttää argumenttien arvoja ohjelmassa esimerkiksi `args.nimi` ja `args.ika`

Alla on esimerkki TypeScript-koodista, jossa luetaan komentorivin argumentteja ja tulostetaan ne konsoliin:

```TypeScript
import * as arg from 'arg'; 

const args = arg({ '--nimi': String, '--ika': Number });

console.log(args.nimi); // tulostaa käyttäjän antaman nimen
console.log(args.ika); // tulostaa käyttäjän antaman iän
```

Ja tässä on mitä tapahtuu, kun suoritat ohjelmaa komentorivillä:

`$ node tiedoston_nimi.js --nimi Johanna --ika 27`

Tulostus:

`Johanna 27`

Ja siinä se on! Komentorivin argumenttien lukeminen TypeScriptilla on nyt helppoa kuin mikä!

## Syvempi sukellus

Kun olet oppinut lukemaan komentorivin argumentteja TypeScriptilla, voit alkaa tehdä monimutkaisempia ohjelmia, joissa otetaan huomioon erilaiset käyttäjän antamat parametrit. Voit myös kokeilla erilaisia argumenttien lukemisen kirjastoja ja löytää ne, jotka sopivat parhaiten tarpeisiisi.

Kokeile myös käyttää argumenttiluettelon määrittelyssä erilaisia arvotyyppejä, kuten Boolean, Array tai Object, riippuen siitä, millaisia tietoja haluat käyttäjän antavan.

## Katso myös

- [arg-kirjaston dokumentaatio](https://www.npmjs.com/package/arg)
- [TypeScriptin perusteet](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html)
- [Komentorivin argumenttien lukeminen muiltakin ohjelmointikieliltä](https://www.digitalocean.com/community/tutorials/about-command-line-arguments-and-flags#other-languages)