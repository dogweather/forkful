---
title:                "Komennoriviparametrien lukeminen"
aliases:
- /fi/typescript/reading-command-line-arguments/
date:                  2024-01-20T17:57:09.153762-07:00
model:                 gpt-4-1106-preview
simple_title:         "Komennoriviparametrien lukeminen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Komennoriviargumenttien lukeminen tarkoittaa käyttäjän antamien syötteiden noutamista komentoriviltä ohjelman suorituksen aikana. Ohjelmoijat tekevät tämän, jotta voivat muokata ohjelmansa toimintaa lennosta tai antaa input-arvoja ilman kovan koodauksen tarvetta.

## How to:
```TypeScript
// tiedosto: greet.ts
const arguments = process.argv.slice(2);
console.log(`Hei ${arguments[0]}!`);

// Komentoriviltä ajettuna:
// $ ts-node greet.ts Tero
// Output: Hei Tero!
```

## Deep Dive
Komentoriviargumenttien lukeminen on ollut osa ohjelmoinnin perustoimintoja siitä asti, kun käyttöjärjestelmät alkoivat tukea komentorivikieltoja. Node.js:ssä `process.argv` on standarditapa päästä käsiksi näihin argumentteihin. Taulukon ensimmäiset kaksi arvoa ovat node-binääritiedoston polku ja ajettavan skriptin polku, joten oikeat argumentit löytyvät taulukon indeksistä 2 eteenpäin.

Vaihtoehtoisesti voimme käyttää kirjastoja, kuten `yargs` tai `commander`, jotka tarjoavat enemmän toiminnallisuutta ja helpottavat argumenttien käsittelyä.

```TypeScript
// Esimerkki yargs-kirjaston käytöstä
import yargs from 'yargs';

const args = yargs(process.argv.slice(2)).argv;
console.log(`Hei ${args.name}!`);

// Komentoriviltä:
// $ ts-node greet.ts --name=Tero
// Output: Hei Tero!
```

Argumenttien lukeminen TypeScriptissä tapahtuu Node.js:n tavoin, mutta lisäetuna on, että voimme hyödyntää TypeScriptin tyypityksiä vähentääksemme virheiden määrää ja parantaaksemme koodin selkeyttä.

## See Also
- [Node.js process.argv dokumentaatio](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Yargs GitHub-sivusto](https://github.com/yargs/yargs)
- [Commander.js GitHub-sivusto](https://github.com/tj/commander.js)
