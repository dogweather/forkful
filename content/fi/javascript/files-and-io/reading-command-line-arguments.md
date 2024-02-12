---
title:                "Komennoriviparametrien lukeminen"
aliases:
- /fi/javascript/reading-command-line-arguments/
date:                  2024-01-20T17:56:57.094075-07:00
model:                 gpt-4-1106-preview
simple_title:         "Komennoriviparametrien lukeminen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä ja Miksi?)
Komennorivin argumentit ovat ohjelmalle annettuja syötteitä, jotka määritellään ohjelman käynnistyksen yhteydessä. Ohjelmoijat käyttävät niitä, jotta voivat räätälöidä ohjelman suoritusta lennossa ilman koodin muokkaamista.

## How to: (Kuinka Tehdään:)
JavaScriptissä Node.js- ympäristössä komennorivin argumenttien lukeminen tapahtuu `process.argv`-objektilla. Perus käyttötapa näyttää tältä:

```javascript
// process_args.js
process.argv.forEach((val, index) => {
  console.log(`${index}: ${val}`);
});

// Käynnistä komennolla:
// node process_args.js moikka maailma
```

Esimerkin tulostus:

```
0: /path/to/node
1: /path/to/process_args.js
2: moikka
3: maailma
```

Argumentit alkavat indeksistä 2, koska Node.js asettaa prosessin ja tiedostopolun ensimmäisiin paikkoihin.

## Deep Dive: (Syväsukellus:)
Komennorivin argumenttien lukeminen onnistuu periaatteessa kaikissa ohjelmointikielissä, mutta tapa ja yksityiskohdat vaihtelevat. Historiallisesti tämä oli yksi varhaisimmista tavoista välittää tietoa ohjelmalle. Nykyään on olemassa vaihtoehtoisia menetelmiä, kuten ympäristömuuttujat ja erilliset konfiguraatiotiedostot. Node.js:ssä `process.argv` on yksinkertainen ja suoraviivainen, mutta isommissa sovelluksissa yleensä käytetään kirjastoja kuten `yargs` tai `commander` tarkemman syntaksin ja virheenkäsittelyn tarjoamiseksi. Argumenttien alkiot ovat aina merkkijonoja, joten niiden käsittelyyn voi kuulua muuntamista toisentyyppisiksi arvoiksi.

## See Also: (Katso Myös:)
- [Node.js documentation for process.argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Yargs GitHub repository](https://github.com/yargs/yargs)
- [Commander.js GitHub repository](https://github.com/tj/commander.js)
- [12 Factor App's take on Config](https://12factor.net/config)
