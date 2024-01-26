---
title:                "YAML-tiedostojen käsittely"
html_title:           "Arduino: YAML-tiedostojen käsittely"
simple_title:         "YAML-tiedostojen käsittely"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
YAML on helppolukuinen datan serialisointiformaatti konfiguraatioihin ja viestintään. Ohjelmoijat käyttävät sitä, koska se on selkeä ja ihmiselle luettavissa, ja sopii hyvin asetustiedostoihin ja datan tallentamiseen.

## How to:
Gleam vielä ei tarjoa vakiona YAML-tukea, mutta voit käyttää ulkopuolisia kirjastoja tai käsitellä YAML:ää merkkijonoina. Esimerkiksi:

```gleam
external fn parse_yaml(String) -> Result(Map(String, String), Nil) =
  "yaml_parser" "parse"

pub fn main() {
  let config_data = "
  port: 8080
  env: production
  "

  let config = parse_yaml(config_data)
  case config {
    Ok(settings) -> settings
    Error(_) -> Map.empty()
  }
}
```

Otetaan oletus, että `yaml_parser` on kuvitteellinen Gleam-kirjasto ja `parse_yaml` palauttaa joko mappauksen tai tyhjän tietorakenteen.

## Deep Dive (Syväsukellus)
YAML syntyi vuonna 2001, ja se on JSONin sukulainen, mutta ihmisläheisempi. Vaihtoehtoja ovat esimerkiksi JSON ja XML. Gleamissa YAML-käsittely riippuu ulkopuolisista kirjastoista – virallista tukea ei toistaiseksi ole. Oleellista on tiedostaa, ettei kaikki kirjastot tue kaikkia YAML-ominaisuuksia, kuten monimutkaisia ankkureita tai laajennuksia.

## See Also (Katso Myös)
- YAML-spesifikaatio: [https://yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)
