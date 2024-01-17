---
title:                "Virheenkorjaustulostuksen tulostaminen"
html_title:           "TypeScript: Virheenkorjaustulostuksen tulostaminen"
simple_title:         "Virheenkorjaustulostuksen tulostaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Pyörittely tulostuksen avulla on työkalu, jota ohjelmoijat käyttävät virheiden paikantamiseen sovelluksiensa koodissa. Pienellä koodinpätkällä voit tulostaa debug-tietoja konsoliin selventämään, miksi ohjelma ei toimi halutulla tavalla.

## Mitä tehdä?
Käytä TypeScriptiä ```console.log()```- toiminnon avulla tulostamaan sisäänrakennettua testimateriaalia. Tämä auttaa selvittämään, missä koodissa on ongelma ja miten sen voi ratkaista.

Esimerkki:
```TypeScript
const nimi = "Pekka";
console.log("Hei, olen " + nimi); //tulostaa: Hei, olen Pekka 
```

## Syvemmälle aiheeseen
Ennen oli tapana käyttää ```alert()```-metodia debug-tulostukseen. Kuitenkin tältä menetelmältä puuttuu tiettyjä hyödyllisiä toimintoja, kuten esimerkiksi tietojen tarkan sijainnin tulostaminen. Tulostuksen sijaan voit myös käyttää debuggeria tutkiaksesi tarkemmin koodin tuloksia.

## Katso myös
- [TypeScript Console API - Microsoft Docs](https://docs.microsoft.com/en-us/scripting/javascript/reference/console-object-javascript) 
- [Debugging in TypeScript - TypeScript Deep Dive](https://basarat.gitbook.io/typescript/type-system#debugging)