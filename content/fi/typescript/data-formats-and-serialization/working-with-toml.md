---
title:                "Työskentely TOML:n kanssa"
aliases:
- /fi/typescript/working-with-toml/
date:                  2024-01-26T04:27:02.978089-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely TOML:n kanssa"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/working-with-toml.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
TOML, lyhenne sanoista Tom's Obvious, Minimal Language, on datan serialisointiformaatti, joka on samankaltainen kuin JSON tai YAML. Ohjelmoijat käyttävät sitä sen ihmisen luettavuuden ja suoraviivaisen datatyyppien kartoituksen vuoksi, mikä tekee siitä suositun valinnan konfiguraatiotiedostoille ja datan vaihdolle.

## Kuinka:
Ensimmäiseksi tarvitset TOML-jäsentimen. `@iarna/toml` on suosittu valinta. Asenna se npm:n kautta: `npm install @iarna/toml --save`. Näin luet TOML-tiedoston ja jäsenät sen JavaScript-objektiksi:

```typescript
import * as fs from 'fs';
import toml from '@iarna/toml';

const tomlContent = fs.readFileSync('config.toml', 'utf-8');
const parsedData = toml.parse(tomlContent);

console.log(parsedData);
```
Jos `config.toml` sisältää:
```
[server]
port = 8080
```
Tulos olisi:
```
{ server: { port: 8080 } }
```
Ja, kirjoittaminen TOML-tiedostoon on yhtä suoraviivaista:
```typescript
import * as fs from 'fs';
import { stringify } from '@iarna/toml';

const obj = { server: { port: 8080 } };
const tomlString = stringify(obj);
fs.writeFileSync('config.toml', tomlString);
``` 
Tämän koodin suorittaminen kirjoittaa objektin `config.toml` tiedostoon TOML-muodossa.

## Syväsukellus
Tom Preston-Werner, GitHubin perustajaosakas, loi TOML:n vuonna 2013 vastauksena hänen havaitsemiinsa puutteisiin muiden formaattien, kuten INI:n tai YAML:n, kanssa. Se on suunniteltu olemaan yksiselitteinen ja helposti jäsenettävä datarakenteisiin, siksi suosittu konfiguraatiotiedostoissa. Vaihtoehdot, kuten JSON, puuttuvat kommenteista, kun taas YAML on monimutkaisempi. TOML loistaa yksinkertaisuudessaan ja kyvyssään edustaa selkeästi monimutkaisia datarakenteita.

Kun jäsenet TOML:ää TypeScriptillä, käännät tekstuaalista dataa rakenteelliseen muotoon, jota kieli voi käsitellä. Tämä sisältää leksikoinnin (raakatekstin muuntaminen tokeneiksi) ja jäsentämisen (sisäisen datarakenteen rakentaminen); `@iarna/toml` käsittelee molemmat saumattomasti. Emojituki on hauska lisä, joka osoittaa TOML:n käyttäjäkeskeistä lähestymistapaa.

## Katso myös
- TOML Virallinen Spesifikaatio: https://toml.io/en/
- `@iarna/toml` paketti: https://www.npmjs.com/package/@iarna/toml
- Vertailuja TOML:n, YAML:n ja JSON:n välillä: https://blog.bitsrc.io/choosing-the-right-configuration-file-format-toml-vs-yaml-vs-json-71b5be8968ea
