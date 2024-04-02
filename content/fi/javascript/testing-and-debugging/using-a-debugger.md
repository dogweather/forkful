---
date: 2024-01-26 03:50:17.442064-07:00
description: "T\xE4ss\xE4 on hieman JavaScript-koodia, joka ei toimi odotetulla tavalla:\
  \ ```javascript function buggyMultiply(a, b) { return a + b; // Oho! T\xE4m\xE4\
  n pit\xE4isi olla\u2026"
lastmod: '2024-03-13T22:44:56.954279-06:00'
model: gpt-4-0125-preview
summary: "T\xE4ss\xE4 on hieman JavaScript-koodia, joka ei toimi odotetulla tavalla:\
  \ ```javascript function buggyMultiply(a, b) { return a + b; // Oho! T\xE4m\xE4\
  n pit\xE4isi olla\u2026"
title: "Debuggerin k\xE4ytt\xF6"
weight: 35
---

## Kuinka:
Tässä on hieman JavaScript-koodia, joka ei toimi odotetulla tavalla:

```javascript
function buggyMultiply(a, b) {
    return a + b; // Oho! Tämän pitäisi olla kertolasku, ei yhteenlasku.
}

let result = buggyMultiply(5, 3);
console.log('Tulos:', result);
```

Tuloste on väärä:
```
Tulos: 8
```

Katsotaanpa, miten debuggataan Chrome DevToolsissa:

1. Avaa tämä JS selaimessa.
2. Napsauta oikealla ja valitse "Tutki" avataksesi DevToolsin.
3. Klikkaa "Lähteet"-välilehteä.
4. Etsi koodinpätkäsi tai sivusi ja aseta katkokohta napsauttamalla rivinumeroa `return`-lauseen vieressä.
5. Päivitä sivu laukaistaksesi katkokohdan.
6. Tarkista "Scope"-paneelista paikalliset muuttujat `a` ja `b`.
7. Astu eteenpäin "Astutaan seuraavan funktion yli"-painikkeella.
8. Huomaa bugi `return`-lauseessa.
9. Korjaa koodi:
```javascript
function buggyMultiply(a, b) {
    return a * b; // Korjattu!
}

let result = buggyMultiply(5, 3);
console.log('Tulos:', result);
```

Korjattu tuloste:
```
Tulos: 15
```

## Syväsukellus
Debuggauksen käsite on ollut olemassa tietokoneiden alkuaikoina—legendan mukaan se alkoi, kun tietokoneesta löydettiin yöperhonen 1940-luvulla! Nykyään, JavaScript-debuggerit kuten selaimiin rakennetut työkalut (Chrome DevTools, Firefox Developer Tools) tai IDE:hen integroidut debuggerit (Visual Studio Code, WebStorm) tarjoavat runsaasti ominaisuuksia.

Vaihtoehtoja sisäänrakennetuille debuggereille ovat kolmannen osapuolen työkalut kuten WebStorm tai hyvä vanha `console.log` muuttujatilojen tulostamiseen. Mutta nämä eivät tarjoa debuggereiden reaaliaikaista vuorovaikutusta ja yksityiskohtaista tarkastelua.

Toiminnan yksityiskohtien osalta useimmat debuggerit toimivat samankaltaisesti: ne antavat sinun asettaa katkokohdat, jotka pysäyttävät suorituksen, astua läpi koodin, tarkastella nykyisiä muuttujien tiloja, seurata lausekkeita ja jopa manipuloida arvoja lennossa testatakseen erilaisia skenaarioita.

## Katso Myös
- [Google Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools)
- [Mozilla Developer Network - Firefox Debugger](https://developer.mozilla.org/en-US/docs/Tools/Debugger)
- [Visual Studio Code - Debuggaus](https://code.visualstudio.com/docs/editor/debugging)
