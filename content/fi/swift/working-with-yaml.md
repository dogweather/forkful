---
title:                "Yhteistyössä YAML:n kanssa"
html_title:           "Swift: Yhteistyössä YAML:n kanssa"
simple_title:         "Yhteistyössä YAML:n kanssa"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

Mitä ja miksi?:
YAML on tapa tallentaa ja välittää tietoa tekstimuodossa. Ohjelmoijat käyttävät sitä mm. mahdollistamaan helpon konfiguroinnin ja tiedon jakamisen eri sovellusten välillä.

Miten: 
Esimerkiksi, jos haluat tallentaa listan käyttäjänimiä ja salasanoja, voit käyttää YAML-muotoa seuraavasti: 

```
Swift
# Lista käyttäjänimistä ja salasanoista
käyttäjä1:
  käyttäjänimi: "käyttäjä1"
  salasana: "salasana1"
käyttäjä2:
  käyttäjänimi: "käyttäjä2"
  salasana: "salasana2"
```

Tällöin tiedot tallentuvat helposti luettavaan muotoon ja niitä on helppo käsitellä esimerkiksi ohjelmointikielellä. 

Syväluotaus: 
YAML on syntynyt XML-formaatin haasteiden ja muun monimutkaisuuden vuoksi. Se on myös joustavampi kuin JSON, mutta vaatii hieman enemmän työtä kuin esimerkiksi CSV. On myös olemassa muita tekstipohjaisia formaatteja, kuten TOML ja INI. YAML:n tärkein etu on kuitenkin sen helppolukuisuus ja -kirjoitettavuus. 

Katso myös: 
- [YAML.org](https://yaml.org/) – YAML:n viralliset verkkosivut
- [YAML: Wikipedia](https://fi.wikipedia.org/wiki/YAML) – Lyhyt kuvaus YAML:stä
- [JSON vs YAML](https://medium.com/swlh/yaml-vs-json-41680258ab92) – Vertailu YAML:n ja JSON:n välillä