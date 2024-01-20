---
title:                "Tekstitiedoston lukeminen"
html_title:           "Lua: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Lukemalla Tekstitiedostot JavaScriptilla: Helppo Opas Programmoinnin Ystäville

## Mikä & Miksi?
Lukemalla tekstitiedosto tarkoittaa tulkkaukseen ja käsittelyyn tietojen eri riveiltä. Programmoinnissa teemme tämän automatisoitujen prosessien, kuten tiedon parsinnan ja analysoinnin, mahdollistamiseksi.

## Miten:
JavaScript-funktion Node.js `fs` (filesystem) avulla voimme lukea tekstitiedostoja. Seuraavassa on esimerkkinä.

```Javascript 
const fs = require('fs'); 

fs.readFile('testi.txt', 'utf8', function(err, data){ 
    if (err) throw err; 
    console.log(data); 
}); 
```
Tämä koodi lukee `testi.txt` tiedoston ja tulostaa sisällön konsoliin. Jos tiedostoa ei ole tai se ei voi avata syystä tai toisesta, se heittää virheen.

## Syvällinen Sukeltaminen
Aikaisemmin, `XMLHttpRequest` -objektia käytettiin tiedostojen lukemiseen selaimessa. Nykyään Fetch API on suositumpi ja modernimpi. Sitä voidaan käyttää tekstitiedostoja lukemaan sekä palvelimelta että paikallisesta järjestelmästä, ja se tukee myös lupauksia. Node.js:n `fs`-moduuli on edelleen standardi tiedostojen lukuun ja kirjoitukseen Node.js-sovelluksissa.

On myös vaihtoehtoja, kuten `readFileSync`, joka on synkroninen versio `readFile`:sta. Käyttö riippuu tarpeistasi. Asynkroninen luku on hyvä, kun et halua prosessin odottavan luvun päättymistä, mutta jos sinun on odotettava tiedoston lukemista, ennen kuin voit jatkaa, synkroninen versio voi olla parempi.

## Katso myös
1. [Node.js FileSystem API documentation](https://nodejs.org/api/fs.html)
2. [Utf8 -koodauksen ymmärtäminen JavaScriptissa](https://developer.mozilla.org/en-US/docs/Web/API/TextEncoder)
3. [Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch)

Loppujen lopuksi, JavaScriptin avulla voimme manipuloida tekstitiedostoja monin eri tavoin. Käytä yllä olevia lähteitä hyödyksi ja jatka koodaamista!