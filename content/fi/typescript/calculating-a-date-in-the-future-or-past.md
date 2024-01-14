---
title:    "TypeScript: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi laskea tulevaisuuden tai menneisyyden päivämäärän? Joskus ohjelmoinnissa on tarpeen tietää mikä päivämäärä on esimerkiksi yhden viikon tai kuukauden päässä. Tämä voi olla hyödyllistä esimerkiksi tapahtumien suunnittelussa tai laskutuksessa.

## Miten

```TypeScript
// Määritetään funktio, joka laskee tietyn päivämäärän tulevaisuudessa
const getDateInFuture = (days: number): Date => {
  // Luodaan uusi Date-objekti nykyisestä päivämäärästä
  let futureDate = new Date(); 
  // Lisätään annettu määrä päiviä nykyiseen päivämäärään
  futureDate = futureDate.setDate(futureDate.getDate() + days); 
  // Palautetaan tulevaisuuden päivämäärä
  return new Date(futureDate);
}

// Kutsutaan funktiota ja annetaan parametrina haluttu päivien määrä
const futureDate = getDateInFuture(7); 

// Tulostetaan tulevaisuuden päivämäärä konsoliin
console.log(futureDate); // Output: 2021-06-22T10:55:33.354Z

// Määritetään funktio, joka laskee tietyn päivämäärän menneisyydessä
const getDateInPast = (months: number): Date => {
  // Luodaan uusi Date-objekti nykyisestä päivämäärästä
  let pastDate = new Date();
  // Vähennetään annettu määrä kuukausia nykyisestä päivämäärästä
  pastDate = pastDate.setMonth(pastDate.getMonth() - months);
  // Palautetaan menneisyyden päivämäärä
  return new Date(pastDate);
}

// Kutsutaan funktiota ja annetaan parametrina haluttu kuukausien määrä
const pastDate = getDateInPast(3);

// Tulostetaan menneisyyden päivämäärä konsoliin
console.log(pastDate); // Output: 2021-03-14T10:55:33.354Z
```

## Syväsukellus

Calculating a Date In The Future or Past -toiminnallisuus voidaan toteuttaa monella eri tavalla TypeScriptissä. Yllä olevat esimerkit käyttävät Date-objektin sisäänrakennettuja metodeja, mutta voit myös käyttää esimerkiksi Moment.js-kirjastoa, jos haluat lisää toiminnallisuuksia ja vaihtoehtoja tulevaisuuden ja menneisyyden päivämäärän laskemiseen.

## Katso myös

- [Date-olion dokumentaatio](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js-kirjaston dokumentaatio](https://momentjs.com/docs/)