---
title:                "TypeScript: Kirjoittaminen standardivirheelle"
simple_title:         "Kirjoittaminen standardivirheelle"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi kirjoittaa standardi virheen lähteenä?

Kirjoittaminen standardi virheen lähteenä on tärkeää ohjelmoijille, jotka haluavat parantaa ohjelmansa virheenhallintaa. Se mahdollistaa ohjelman suorittamisen aikana ilmenevien virheiden jäljittämisen ja korjaamisen. Ilman tätä toimintoa, olisi vaikeaa löytää ja korjata kyseisiä virheitä, mikä johtaisi ohjelman laadun huononemiseen.

## Miten kirjoittaa standardi virheen lähteenä?

```TypeScript
console.error("Tämä on virheilmoitus standardi virheen lähteestä")
```

Tämä yksinkertainen koodiesimerkki näyttää, miten kirjoitat standardi virheen lähteenä TypeScript-ohjelmassa. Kun ohjelma suoritetaan, virheen ilmoitus näkyy terminaalissa, joka auttaa ohjelmoijaa paikantamaan virheen ja korjaamaan sen.

## Syväsukellus standardi virheen lähteeseen

Kirjoittaminen standardi virheen lähteenä käyttäen console.error()-funktiota on erittäin hyödyllistä virheenhallinnassa. Tämä toiminto tulostaa virheen viestin ja lisää sen virhelokiksi, joka helpottaa virheen jäljittämistä myöhemmin. Tämän lisäksi voidaan myös lisätä virheen tiettyjä tietoja, kuten aikaleima tai virhekoodi, jotta virheen syy voidaan selvittää nopeammin.

## Katso myös

- [Console API TypeScriptissä] (https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-3.html#console-assert)
- [Debuggauksen perusteet TypeScript:ssä] (https://www.pluralsight.com/guides/getting-started-with-typescript-debugging)