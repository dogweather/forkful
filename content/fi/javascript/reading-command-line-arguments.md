---
title:                "Javascript: Komentoriviparametrien lukeminen"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Miksi käsitellä komentoriviparametreja?

Komentoriviparametrit ovat tärkeä osa Javascript-ohjelmointia, sillä ne antavat käyttäjille mahdollisuuden antaa lisäargumentteja koodille. Tämä mahdollistaa ohjelmien räätälöinnin ja käyttökokemuksen parantamisen.

Kuinka käsitellä komentoriviparametreja:

```javascript
// Esimerkki koodista
// Tämä koodi tulostaa komentoriviltä annetun argumentin
let parametri = process.argv[2];
console.log(`Käyttäjän antama parametri: ${parametri}`);
```

Kun ohjelma suoritetaan komentoriviltä komennolla "node ohjelma.js argumentti", tulostuu seuraava:
```
Käyttäjän antama parametri: argumentti
```

Syötettyä parametria voi käyttää koodissa esimerkiksi ehtolausekkeessa tai laskutoimituksissa. Parametrien käsittely avaa mahdollisuuden luoda monipuolisempia ja interaktiivisempia ohjelmia.

Syrjäytys:

Komentoriviparametrit ovat käytännöllisiä työkaluja, mutta koodin lukuisien parametrien käsittely voi näyttäytyä haastavalta. On tärkeää ymmärtää, että prosessi.argv-taulukko sisältää myös muita objekteja, kuten tiedoston nimen ja suorituskerrat. Koodin suunnittelussa onkin hyvä pohtia, miten prosessi.argv-taulukkoa voidaan hyödyntää tehokkaimmin.

## Katso myös
- [MDN: Command line arguments](https://developer.mozilla.org/fi/docs/Web/API/CommandLine)