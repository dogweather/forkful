---
title:                "Väliaikaistiedoston luominen"
html_title:           "TypeScript: Väliaikaistiedoston luominen"
simple_title:         "Väliaikaistiedoston luominen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?

Luotaessa ohjelmia, kehittäjät voivat joutua luomaan väliaikaisia tiedostoja, jotka ovat tarpeellisia joko ohjelmantekijän tai ohjelman toiminnan kannalta.

Väliaikainen tiedosto on lyhytaikainen tiedosto, jota käytetään ohjelman aikana ja se poistetaan käytön jälkeen. Tästä syystä luominen ja poistaminen tulee tehdä jokaisen käytön yhteydessä ja tämä on yleinen tapa käsitellä ohjelman tarvitsemia väliaikaisia tietoja.

# Miten:

```TypeScript
import { tmpFile, writeFile, deleteFile } from 'fs';

tmpFile((err, path) => {
    if (err) throw err;
    let data = "Tämä on väliaikainen tiedosto!";
    writeFile(path, data, (err) => {
        if (err) throw err;
        console.log("Tiedoston kirjoittaminen onnistui!");
        deleteFile(path, (err) => {
            if (err) throw err;
            console.log("Tiedoston poistaminen onnistui.");
        });
    });
});
```

Tässä esimerkissä käytämme Node.js:ään kuuluvaa ```fs``` kirjastoa luodaksemme, kirjoittaaksemme ja poistaaksemme väliaikaisen tiedoston. Ensin käyttäjän täytyy koodissa määritellä polku, johon väliaikainen tiedosto tallennetaan ```tmpFile``` funktion avulla. Sitten voidaan kirjoittaa tiedostoon haluttu data ```writeFile``` funktion avulla. Lopuksi poistamme tiedoston ```deleteFile``` funktion avulla.

# Syvä sukellus:

Väliaikaisten tiedostojen käyttö on ollut yleinen tapa ohjelmoidessa jo vuosikymmenten ajan. Ensimmäiset käyttöjärjestelmät käyttivät tätä metodia tiedostojenhallintaan ja viime vuosina tästä on tullut standardi tapa käsitellä väliaikaisia tietoja myös ohjelmointikielet, kuten TypeScript, maailmassa.

On myös muita tapoja luoda väliaikaisia tiedostoja, kuten käyttämällä ```tempfile``` kirjastoa tai hyödyntämällä tietokantoja, mutta ```fs``` kirjaston käyttö on yleinen ja toimiva tapa.

Väliaikaisten tiedostojen luominen ja poistaminen voi myös vaatia käyttöjärjestelmän lupaa, erityisesti jos se tapahtuu jatkuvasti. On tärkeää huolehtia, että väliaikaiset tiedostot poistetaan asianmukaisesti, jotta ne eivät vie turhaan tilaa tai aiheuta turhia tietoturvariskejä.

# Katso myös:

[Lisätietoa TypeScript-kielestä](https://www.typescriptlang.org/)

[Node.js ```fs``` kirjastodokumentaatio](https://nodejs.org/api/fs.html)

["tempfile" kirjasto](https://www.npmjs.com/package/tempfile)