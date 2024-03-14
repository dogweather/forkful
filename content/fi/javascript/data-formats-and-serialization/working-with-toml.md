---
date: 2024-01-26 04:23:29.312415-07:00
description: "TOML, lyhenne sanoista Tom\u2019s Obvious, Minimal Language, m\xE4\xE4\
  rittelee, miten konfiguraatiotiedostot rakennetaan. Ohjelmoijat k\xE4ytt\xE4v\xE4\
  t TOML:\xE4\xE4, koska se on\u2026"
lastmod: '2024-03-13T22:44:56.973253-06:00'
model: gpt-4-0125-preview
summary: "TOML, lyhenne sanoista Tom\u2019s Obvious, Minimal Language, m\xE4\xE4rittelee,\
  \ miten konfiguraatiotiedostot rakennetaan. Ohjelmoijat k\xE4ytt\xE4v\xE4t TOML:\xE4\
  \xE4, koska se on\u2026"
title: "Ty\xF6skentely TOML:n kanssa"
---

{{< edit_this_page >}}

## Mikä & Miksi?
TOML, lyhenne sanoista Tom’s Obvious, Minimal Language, määrittelee, miten konfiguraatiotiedostot rakennetaan. Ohjelmoijat käyttävät TOML:ää, koska se on helppolukuinen, helppo kirjoittaa ja kuvastaa hyvin hajautustaulua, mikä tekee siitä suosikin konfiguraatioissa.

## Kuinka:
Käyttääksesi TOML:ää JavaScriptissä, tarvitset parserin kuten `@iarna/toml`. Ensin, asenna se: `npm install @iarna/toml`. Sen jälkeen, muunna TOML-merkkijono JavaScript-objektiksi tai JavaScript-objekti TOML-muotoon.

```javascript
const toml = require('@iarna/toml');

// Muunna TOML-merkkijono JS-objektiksi
const tomlStr = `
title = "TOML Esimerkki"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
`;

const parsedData = toml.parse(tomlStr);
console.log(parsedData);

// Muunna JS-objekti TOML-merkkijonoksi
const jsObject = {
  title: "TOML Esimerkki",
  database: {
    server: "192.168.1.1",
    ports: [8001, 8001, 8002]
  }
};

const tomlString = toml.stringify(jsObject);
console.log(tomlString);
```

## Syväsukellus
TOML julkaistiin ensimmäisen kerran vuonna 2013 Tom Preston-Wernerin toimesta, joka on yksi GitHubin perustajista. Se suunniteltiin korvaamaan muita formaatteja, kuten INI, olemalla enemmän standardisoitu ja helpommin jäsentänyt. JSON ja YAML ovat vaihtoehtoja, mutta ne voivat olla liian monimutkaisia tai liian joustavia. TOML: n etuna on staattinen konfiguraatio, jossa yksinkertainen, selkeä formaatti on suositeltavaa. Sen suunnittelu mahdollistaa suoraviivaisen kartoituksen hajautustauluun, avainten ja arvojen vastatessa ominaisuusnimiä ja niiden arvoja. Laajempaan käyttöön saattaa olla tarpeen integroida työkaluja, jotka voivat muuntaa TOML:n ja muiden formaattien välillä johtuen vaihtelevasta ekosysteemituesta.

## Katso Myös
- Virallinen TOML GitHub-repositorio: https://github.com/toml-lang/toml
- TOML vs. YAML vs. JSON vertailu: https://gist.github.com/oconnor663/9aeb4ed56394cb013a20
- npm `@iarna/toml` paketti: https://www.npmjs.com/package/@iarna/toml
