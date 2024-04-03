---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:22.575513-07:00
description: "Kuinka: Assosiatiivisten taulukoiden luominen ja k\xE4ytt\xE4minen TypeScriptiss\xE4\
  \ on suoraviivaista. T\xE4ss\xE4 perusohjeet."
lastmod: '2024-03-13T22:44:56.308558-06:00'
model: gpt-4-0125-preview
summary: "Assosiatiivisten taulukoiden luominen ja k\xE4ytt\xE4minen TypeScriptiss\xE4\
  \ on suoraviivaista."
title: "Assosiatiivisten taulukoiden k\xE4ytt\xF6"
weight: 15
---

## Kuinka:
Assosiatiivisten taulukoiden luominen ja käyttäminen TypeScriptissä on suoraviivaista. Tässä perusohjeet:

```TypeScript
// Assosiatiivisen taulukon julistaminen
let user: { [key: string]: string } = {};

// Datan lisääminen
user["name"] = "Jane Doe";
user["email"] = "jane@example.com";

console.log(user);
```

Tuloste:

```TypeScript
{ name: 'Jane Doe', email: 'jane@example.com' }
```

Avain-arvo-parien iterointi on myös helppoa:

```TypeScript
for (let key in user) {
    console.log(key + ": " + user[key]);
}
```

Tuloste:

```TypeScript
name: Jane Doe
email: jane@example.com
```

Ja jos käsittelet sekalaista datatyyppejä, TypeScriptin tyyppijärjestelmä tulee avuksi:

```TypeScript
let mixedTypes: { [key: string]: string | number } = {};
mixedTypes["name"] = "John Doe";
mixedTypes["age"] = 30;

console.log(mixedTypes);
```

Tuloste:

```TypeScript
{ name: 'John Doe', age: 30 }
```

## Syväsukellus
TypeScriptissä meidän viittaamat assosiatiiviset taulukot ovat käytännössä olioita. Historiallisesti kielissä kuten PHP:ssä assosiatiiviset taulukot ovat olleet perustyyppi, mutta JavaScript (ja sen laajennus, TypeScript) käyttää tähän tarkoitukseen olioita. Tämä lähestymistapa on sekä vahvuus että rajoitus. Oliot tarjoavat dynaamisen rakenteen merkkijonojen yhdistämiseen arvoihin, mutta niitä ei ole tarkoitettu käytettäväksi 'taulukoina' perinteisessä mielessä. Esimerkiksi et voi suoraan käyttää taulukkometodeja kuten `push` tai `pop` näissä olioissa.

Tapauksissa, joissa tarvitset järjestettyjä avain-arvo-parikokoelmia taulukkomaisilla toiminnoilla, TypeScript (ja moderni JavaScript) tarjoaa `Map`-objektin:

```TypeScript
let userMap = new Map<string, string>();
userMap.set("name", "Jane Doe");
userMap.set("email", "jane@example.com");

userMap.forEach((value, key) => {
    console.log(key + ": " + value);
});
```

Vaikka TypeScriptin tyyppijärjestelmä ja ES6-ominaisuudet, kuten `Map`, tarjoavat tehokkaita vaihtoehtoja, olioiden käytön ymmärtäminen assosiatiivisina taulukoina on hyödyllistä skenaarioissa, joissa olioliteraalit ovat tehokkaampia tai kun työskentelet JSON-tietorakenteiden kanssa. Kyse on oikean työkalun valitsemisesta tehtävään.
