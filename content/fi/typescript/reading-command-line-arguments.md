---
title:    "TypeScript: Komentoriviparametrien lukeminen"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Miksi

Komentoriviparametrien lukeminen on olennainen osa minkä tahansa TypeScript-sovelluksen kirjoittamista. Se mahdollistaa ohjelman käytön muokkaamisen ilman koodin uudelleenkäännöstä.

## Miten

Komentoriviparametrien lukeminen voidaan toteuttaa yksinkertaisesti käyttämällä TypeScript-komentoriviparametriä sisäänrakennetulla "process" -moduulilla. Alla on esimerkki koodista ja tulosteesta, jossa luetellaan kaikki annetut komentoriviparametrit.

```TypeScript
import process from 'process';

// Tämä funktio ottaa vastaan ​​kaksi komentoriviparametria ja palauttaa niiden summan
function sum(parameter1: number, parameter2: number) {
    return parameter1 + parameter2;
}

// Tallennetaan komentoriviparametrit muuttujiin
const num1 = Number(process.argv[2]);
const num2 = Number(process.argv[3]);

// Tulostetaan summa
console.log(sum(num1, num2));

// Komentorivi: node index.ts 5 7
// Tuloste: 12
```

## Syvemmälle

Komentoriviparametrien lukeminen ei rajoitu vain numeroiden lukemiseen, vaan se voi myös olla joustava työkalu käyttäjille antaa muuttujien arvoja ja asetuksia ohjelman suorittamisen yhteydessä. Komentoriviparametreja voidaan käyttää esimerkiksi määrittämään tietokantayhteys, asettamaan kieliasetukset tai antamaan tulostiedoston nimi.

Olemme jo käyttäneet "process" -moduulia tässä esimerkissä, mutta sen lisäksi TypeScript tarjoaa myös muita vaihtoehtoja komentoriviparametrien käsittelyyn, kuten "yargs" ja "commander" -kirjastot.

## Katso myös

- [Node.js process.argv dokumentaatio](https://nodejs.org/api/process.html#process_process_argv)
- [yargs kirjaston dokumentaatio](https://www.npmjs.com/package/yargs)
- [commander kirjaston dokumentaatio](https://www.npmjs.com/package/commander)