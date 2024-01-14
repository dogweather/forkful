---
title:                "Gleam: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi

YAML on ohjelmointikieletön tiedostomuoto, jota käytetään usein määrittelemään tietorakenteita ja asetuksia. Sen avulla voit tallentaa tiedot ihmisystävällisessä muodossa, mikä tekee siitä loistavan vaihtoehdon tiedostojen tallentamiseen ja lähettämiseen. Gleamissa YAML on kätevä tapa lukea ja parsia tiedostoja ja käsitellä niiden sisältöä. 

## Kuinka

### Lukeminen
Gleamilla voit lukea YAML-tiedostoja helposti käyttämällä bibliotekkia `gleam_yaml`.

```
Gleam |> Yaml.encode(viesti, polku)
```

Tämä koodi luo YAML-merkkijonon `viesti`-muuttujasta ja tallentaa sen `polku`-polkuun.

### Parsiminen
Gleamilla voit myös helposti parsia YAML-tiedostoja ja käsitellä niiden sisältöä koodissasi. 

```
case Gleam |> Yaml.decode(data) {
    Ok(parsed) -> // parsed on YAML-tiedoston sisältämä data
    Error(e) -> // käsittely virheelliselle tiedostolle
}
```

Tässä esimerkissä käytämme `Gleam |> Yaml.decode(data)` -funktiota, joka palauttaa joko `Ok`-arvon käsiteltävästä datasta tai `Error`-arvon jos `data` ei ole validi YAML-tiedosto. Tämän avulla voit helposti käsitellä tiedoston sisältöä esimerkiksi rakenteiden avulla.

## Syvä Syöksy

Vaikka Gleamin `gleam_yaml` bibliotekki tekee YAML-tiedostojen lukemisen ja parsimisen melko helpoksi, on silti hyödyllistä tietää tarkemmin YAML-syntaksista ja sen eri ominaisuuksista. Voit lukea lisää YAML:n ominaisuuksista ja syntaksista [YAML-spesifikaatiosta](https://yaml.org/spec/) ja kokeilla erilaisia lähestymistapoja käsitellä YAML-tiedostoja omassa koodissasi.

## Katso Myös

- [Gleamin virallinen dokumentaatio](https://gleam.run/)
- [YAML-spesifikaatio](https://yaml.org/spec/)
- [Gleamin `gleam_yaml` bibliotekin dokumentaatio](https://hexdocs.pm/gleam_yaml/readme.html)