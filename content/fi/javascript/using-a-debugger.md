---
title:                "Debuggerin käyttö"
date:                  2024-01-26T03:50:17.442064-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debuggerin käyttö"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/using-a-debugger.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Debuggerin käyttö tarkoittaa erikoistyökaluihin tutustumista, jotka antavat sinun kurkata koodisi konepellin alle ja seurata sen suoritusta askel askeleelta. Ohjelmoijat tekevät näin paikatakseen bugeja, optimoidakseen suorituskykyä ja ymmärtääkseen koodin käyttäytymistä.

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