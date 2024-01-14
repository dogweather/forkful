---
title:    "TypeScript: Väliaikaisen tiedoston luominen"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi 

Pieniä, väliaikaisia tiedostoja luodaan usein ohjelmoinnissa monista syistä, joista yleisin on tallentaa väliaikaista dataa, jota ohjelma tarvitsee suorittaessaan tietyn toiminnon. Toinen yleinen syy on luoda väliaikainen tiedosto varmuuskopiointia varten. Kuitenkin, oli syy mikä tahansa, TypeScriptillä on helppo tapa luoda nämä väliaikaiset tiedostot.

## Kuinka tehdä

Ennen kuin aloitamme, varmista että sinulla on TypeScript asennettuna ja tiedostopääte `.ts` tiedostoillesi. Seuraa näitä yksinkertaisia vaiheita luodaksesi väliaikaisen tiedoston TypeScriptillä:

1. Tuodaan `fs` moduuli käyttöön, joka mahdollistaa tiedostojen luomisen ja käsittelyn TypeScriptissä:

```TypeScript
import * as fs from "fs";
```

2. Luodaan funktio, joka luo väliaikaisen tiedoston määritellyllä tiedostonimellä ja sisällöllä:

```TypeScript
function createTemporaryFile(filename: string, content: string) {
    fs.writeFile(filename, content, (err) => {
        if (err) console.log(err);
        console.log(`${filename} luotu.`);
    })
}
```

3. Kutsutaan funktiota ja määritellään tiedostonimi ja sisältö:

```TypeScript
createTemporaryFile("testi.txt", "Tämä on väliaikainen tiedosto.");
```

4. Ajetaan koodi komentokehotteella ja tarkistetaan `testi.txt` tiedoston olemassaolo ja sisältö:

```TypeScript
$ tsc temporary_file.ts
$ node temporary_file.js
```

Tämän tulisi luoda `testi.txt` tiedosto ja tulostaa komentokehotteelle viesti `"testi.txt luotu."`.

## Syventävä tieto

Tässä syventävässä osiossa käsittelemme lyhyesti miten TypeScriptin `fs` moduuli toimii tiedostojen luomisen suhteen. Kuten yllä esitetyssä esimerkissä, `fs.writeFile()` funktio ottaa parametreikseen tiedostonimen ja sisällön, ja mahdollisen virheen käsittelyä varten callback funktion. Funktio luo tiedoston annetulla nimellä ja tallentaa annetun sisällön siihen.

On myös hyvä huomata, että `fs.writeFile()` funktio korvaa automaattisesti jo olemassa olevan tiedoston, jos sellainen löytyy annetulla nimellä. Jos haluat estää tämän, voit käyttää `fs.writeFileSync()` funktiota, joka heittää virheen jos tiedosto on jo olemassa.

## Katso myös

- [TypeScriptin virallinen sivusto](https://www.typescriptlang.org/)
- [fs moduulin dokumentaatio](https://nodejs.org/api/fs.html#fs_file_system)
- [Node.js tiedostojen käsittely](https://nodejs.dev/learn/file-system-in-nodejs)