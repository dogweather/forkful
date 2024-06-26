---
date: 2024-01-26 04:27:02.978089-07:00
description: "Kuinka: Ensimm\xE4iseksi tarvitset TOML-j\xE4sentimen. `@iarna/toml`\
  \ on suosittu valinta. Asenna se npm:n kautta: `npm install @iarna/toml --save`.\
  \ N\xE4in luet\u2026"
lastmod: '2024-03-13T22:44:56.338431-06:00'
model: gpt-4-0125-preview
summary: "Ensimm\xE4iseksi tarvitset TOML-j\xE4sentimen."
title: "Ty\xF6skentely TOML:n kanssa"
weight: 39
---

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
