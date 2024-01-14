---
title:    "TypeScript: Päivämäärän muuttaminen merkkijonoksi"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Miksi: Miksi muuntaa päivämäärä merkkijonoksi?

Päivämäärän muuntaminen merkkijonoksi on tärkeä mahdollisuus ohjelmoinnissa, sillä se antaa mahdollisuuden esittää päivämäärää halutussa muodossa. Tämä voi olla erityisen tärkeää sovellusten käyttäjille, jotka voivat olla tottuneita tiettyyn päivämäärämuotoon. Päivämäärän muuttaminen merkkijonoksi voi myös helpottaa päivämääräarvojen käsittelyä, esimerkiksi jos halutaan verrata kahta päivämäärää.

## Miten: Esimerkkikoodia TypeScriptilla

Seuraavassa esimerkissä käytetään Date-oliota ja sen sisäänrakennettua toLocaleString()-metodia päivämäärän muuntamiseen merkkijonoksi. Tämä metodi palauttaa päivämäärän halutussa muodossa, joka on määritelty parametreina. Esimerkissä halutaan esittää päivämäärä muodossa "päivä-kuukausi-vuosi".

```TypeScript
let date = new Date(); // Luodaan uusi Date-olio
let dateString = date.toLocaleString("fi", { day: "numeric", month: "numeric", year: "numeric" }); // Muutetaan päivämäärä merkkijonoksi
console.log(dateString); // Tulostaa esimerkiksi "26.4.2021"
```

Kuten näemme, päivämäärä on nyt muutettu halutussa muodossa merkkijonoksi ja voimme käyttää sitä esimerkiksi tulostamalla sen konsoliin. Voit muokata parametreja haluamallasi tavalla saadaksesi päivämäärän esimerkiksi englanniksi tai haluamassasi muodossa.

## Syväsukellus: Tarkempaa tietoa päivämäärän muuntamisesta merkkijonoksi

Päivämäärän muuntaminen merkkijonoksi voi näyttää helpolta tehtävältä, mutta taustalla on useita tekijöitä, joita kannattaa ottaa huomioon. Ensinnäkin, eri kielissä käytetään eri päivämäärämuotoja ja siksi on tärkeää määrittää oikea kieli ja muoto, jotta muunnos tapahtuu halutulla tavalla.

Toiseksi, Date-olion toLocaleString()-metodi hyödyntää käyttäjän selaimessa olevaa asetusta päivämäärämuodosta ja siksi tulos voi vaihdella eri käyttäjillä. Tämä tulee ottaa huomioon, jos halutaan, että päivämäärät näytetään aina samassa muodossa riippumatta käyttäjän asetuksista.

Lisäksi, päivämäärän muuntaminen merkkijonoksi antaa mahdollisuuden käyttää erilaisia parametreja ja luoda halutunlaisia päivämäärämuotoja. Tästä syystä on tärkeää ymmärtää eri parametrien vaikutus ja kokeilla erilaisia vaihtoehtoja saadaksesi haluamasi lopputuloksen.

## Katso myös

- [Date-olion dokumentaatio](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [toLocaleString()-metodin dokumentaatio](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString)
- [Käyttäjän päivämääräasetusten hallinta](https://support.google.com/chrome/answer/95290?hl=en)