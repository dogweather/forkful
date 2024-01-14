---
title:                "Fish Shell: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi käyttää JSONia ohjelmoinnissa?

JSON (JavaScript Object Notation) on suosittu tapa tallentaa ja siirtää tietoa verkkosovellusten välillä. Se on yksinkertainen, helppolukuinen ja helposti käsiteltävä tietomuoto, joka tekee siitä erinomaisen vaihtoehdon monimutkaisemman XML:n sijaan. Jos haluat oppia käyttämään JSONia Fish Shellin avulla, jatka lukemista.

## Kuinka käyttää JSONia Fish Shellillä?

Ensinnäkin, sinun täytyy varmistaa, että Fish Shell on asennettu tietokoneellesi. Voit tarkistaa tämän kirjoittamalla `fish --version` komentoriville ja näetkö jonkinlaisen vastauksen. Jos ei, voit asentaa Fish Shellin seuraavasti:

```
sudo apt-get install fish
```

Kun olet varmistanut, että Fish Shell on käytössäsi, voit aloittaa JSONin käsittelyn. Ensinnäkin, sinun täytyy ladata JSON-tiedosto. Voit tehdä tämän kirjoittamalla `curl` komennon ja sen perään URL-osoitteen, josta haluat ladata tiedoston. Esimerkiksi:

```
curl https://example.com/data.json
```

Tämän jälkeen voit käyttää Fish Shellin `jq`-työkalua käsittelemään JSONia. `jq` on suorakäyttöinen JSON-parsija ja -manipulaattori, joka on helppo asentaa käyttöjärjestelmästä riippumatta.

```
jq '.key' data.json
```

Tämä komento etsii `data.json` tiedostosta kaikki `key` arvot ja tulostaa ne näytölle. Voit myös käyttää `jq`-työkalua lisätäksesi, muokataksesi tai poistaaksesi tietoja JSON-tiedostosta.

## Syventävä tieto JSONin käsittelystä

Fish Shellissä on muitakin tapoja käsitellä JSONia kuin `jq`-työkalun avulla. Voit myös käyttää `sed`-työkalua tai `cat`-komentoa parsimaan ja muokkaamaan JSONia. Voit myös käyttää `jq`-työkalua yhdessä muiden komentojen, kuten `grep` tai `awk`, kanssa monimutkaisempien tieotojen käsittelyyn.

Jotta voit ottaa täyden hyödyn irti JSONin käytöstä Fish Shellillä, on suositeltavaa tutustua myös muihin tietomuodon ominaisuuksiin ja käyttötapoihin. Voit löytää hyödyllistä tietoa JSONista ja sen käytöstä monista eri lähteistä, kuten Fish Shellin dokumentaatiosta, internetistä ja kirjoista.

## Katso myös

- [Fish Shell dokumentaatio](https://fishshell.com/docs/current/)
- [jq dokumentaatio](https://stedolan.github.io/jq/)
- [JSONin käyttö Fish Shellin kanssa](https://medium.com/@aaron_kaminsky/json-tricks-for-busy-unix-people-435d9332696f)
- [JSON-parsimisen perusteet](https://www.json.org/json-fi.html)