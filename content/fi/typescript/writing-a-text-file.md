---
title:                "TypeScript: Tiedostoon kirjoittaminen"
simple_title:         "Tiedostoon kirjoittaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstitiedostojen kirjoittaminen saattaa vaikuttaa yksinkertaiselta tehtävältä, mutta se on tärkeä osa ohjelmointia. Tekstitiedostoilla voidaan tallentaa tietoa, jota voidaan käyttää myöhemmin tai jakaa muiden ohjelmien kanssa.

## Miten

Käyttämällä TypeScriptia voi helposti kirjoittaa tekstitiedostoja. Ensimmäiseksi on luotava muuttuja, joka sisältää tiedoston nimen ja polun. Tämän jälkeen käytetään writeFile-funktiota, joka ottaa parametreinaan tiedoston nimen, sisällön ja mahdolliset asetukset. Katso esimerkkikoodi alla:

```TypeScript
const tiedostoNimi = "esimerkki.txt";
const sisalto = "Tämä on esimerkki tekstitiedostosta.";

writeFile(tiedostoNimi, sisalto, (err) => {
    if (err) {
        console.log("Virhe tiedoston kirjoittamisessa: " + err);
    } else {
        console.log("Tiedosto kirjoitettu onnistuneesti!");
    }
});
```

Tämän jälkeen ohjelma luo tiedoston nimeltä "esimerkki.txt" ja kirjoittaa siihen sisällöksi "Tämä on esimerkki tekstitiedostosta.". Voit tarkistaa tiedoston sisällön avaamalla sen esimerkiksi tekstieditorilla.

## Syventävä sukellus

Tiedoston kirjoittaminen sisältää muutakin kuin pelkän sisällön luomisen. writeFile-funktion parametreilla voi määrittää myös tiedostolle enkoodauksen tai käyttää writeFileSynch-funktiota, joka kirjoittaa tiedoston synkronisesti eikä tarvitse callback-funktiota. Jokaisella kielellä on omat ominaisuutensa tiedon tallentamisessa ja jakamisessa, joten on hyvä tutustua TypeScriptin dokumentaatioon lisätietojen saamiseksi.

## Katso myös

- [TypeScriptin dokumentaatio](https://www.typescriptlang.org/docs/)
- [FS-moduuli Node.js:ssa](https://nodejs.org/api/fs.html)