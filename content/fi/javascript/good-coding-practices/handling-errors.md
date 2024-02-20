---
date: 2024-01-26 00:53:55.093985-07:00
description: "Virheenk\xE4sittely on tapasi hallita tilanteita, kun koodissasi tapahtuu\
  \ odottamattomia virheit\xE4. Se on keskeist\xE4, koska se auttaa ohjelmiasi kaatumaan\u2026"
lastmod: 2024-02-19 22:05:15.852074
model: gpt-4-1106-preview
summary: "Virheenk\xE4sittely on tapasi hallita tilanteita, kun koodissasi tapahtuu\
  \ odottamattomia virheit\xE4. Se on keskeist\xE4, koska se auttaa ohjelmiasi kaatumaan\u2026"
title: "Virheiden k\xE4sittely"
---

{{< edit_this_page >}}

## Mikä ja Miksi?

Virheenkäsittely on tapasi hallita tilanteita, kun koodissasi tapahtuu odottamattomia virheitä. Se on keskeistä, koska se auttaa ohjelmiasi kaatumaan arvokkaasti ja ohjeistaa käyttäjiä selkeästi, sen sijaan että ne vain kaatuisivat ja lakkaisivat toimimasta.

## Kuinka tehdä:

Tässä on klassinen `try-catch` lohko:

```javascript
try {
  // Koodi, joka voi aiheuttaa virheen
  let tulos = mahdollisestiRiskialtisToiminto();
  console.log('Onnistui:', tulos);
} catch (virhe) {
  // Mitä tehdä, jos virhe tapahtuu
  console.error('Hups:', virhe.viesti);
}
```

Esimerkkituloste, kun virhettä ei tapahdu:
```
Onnistui: 42
```

Ja kun virhe tapahtuu:
```
Hups: Jotain meni pieleen
```

Asynkroniselle koodille, jossa on käytössä promiset, käytä `try-catch` lohkoa `async` funktion yhteydessä:

```javascript
async function haeTietoja() {
  try {
    let data = await fetch('https://api.example.com/data');
    console.log('Tiedot haettu:', data);
  } catch (virhe) {
    console.error('Datan hakuvirhe:', virhe.viesti);
  }
}

haeTietoja();
```

## Syväsukellus

Virheenkäsittely JavaScriptissä on kehittynyt. Aikanaan (ES3, noin 1999) meillä oli käytössämme vain `try-catch` lohko. Ei kovin joustava, mutta teki tehtävänsä.

ES6 (2015) toi mukanaan Promiset ja antoi meille `.then()` ja `.catch()` metodit, joiden avulla pystyimme käsittelemään asynkronisia virheitä sulavammin.

```javascript
fetch('https://api.example.com/data')
  .then(data => console.log('Tiedot haettu:', data))
  .catch(virhe => console.error('Datan hakuvirhe:', virhe.viesti));
```

Toteutuksen yksityiskohdista puhuttaessa, kun virhe heitetään, JavaScript-moottorit luovat `Error` objektin, jolla on hyödyllisiä ominaisuuksia kuten `viesti` ja `pino`. Voit myös luoda omia virhetyyppejä laajentamalla `Error` luokkaa – kätevää monimutkaisemmissa sovelluksissa.

Vaihtoehtoja? Voisit jättää virheenkäsittelyn huomiotta (huono idea), käyttää takaisinkutsuja, joilla on ensimmäisenä parametrina virhe (terveisiä, Node.js tyyli), tai käyttää kehityskirjastoja ja -runkoja, jotka tarjoavat omia ratkaisujaan.

## Katso myös

Lisätietoja virheenkäsittelystä:

- MDN try-catch: [MDN try...catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- Async/Await: [MDN async function](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function)
- Opas Promiselle: [MDN Promises](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)
- Omaa virhetyypin luominen ja heittäminen: [MDN Error](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error)
