---
title:                "Komentoriviparametrien lukeminen"
html_title:           "Javascript: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Komentoriviargumenttien lukeminen tarkoittaa käyttäjän antamien parametrien hakemista ja tallentamista ohjelman suorituksen aikana. Tämä on tärkeää, jotta ohjelma voi välittää käyttäjän antaman tiedon ja suorittaa tarvittavia toimintoja sen perusteella.

## Näin:
```Javascript
// Esimerkki komentoriviargumenttien lukemisesta
const argumentit = process.argv.slice(2); // Palauttaa taulukon komentoriviargumenteista (ilman ensimmäistä kahta automaattisesti annettua argumenttia)
console.log(argumentit); // Tulostaa taulukon sisällön konsoliin
```

**Komentorivillä suoritettu komento:**
```
node ohjelma.js argumentti1 argumentti2
```

**Tulostus konsolissa:**
```
[ 'argumentti1', 'argumentti2' ]
```

## Syväsukellus:
Komentoriviargumenttien lukeminen perustuu Unix-tyyppisten käyttöjärjestelmien "argumenttiväylä"-konseptiin, jossa ohjelman suorituksella on mahdollista käyttää käyttäjän antamia parametreja. Tämä toimintatapa on vakiintunut osa monien ohjelmointikielten, kuten Javascriptin, toimintaympäristöä ja mahdollistaa monipuolisemman käytön. Vaihtoehtoisesti komentoriviargumenttien sijaan voi myös käyttää ympäristömuuttujia, jotka ovat globaalissa muuttujassa nimeltään ```process.env```.

## Katso myös:
- [Node.js process.argv documentation](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Unix command line arguments explanation](https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html)