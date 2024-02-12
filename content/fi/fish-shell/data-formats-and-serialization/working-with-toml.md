---
title:                "Työskentely TOML:n kanssa"
aliases: - /fi/fish-shell/working-with-toml.md
date:                  2024-01-26T04:21:28.129214-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely TOML:n kanssa"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/working-with-toml.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
TOML on konfiguraatiotiedostomuoto, joka on helppo ihmisille lukea ja kirjoittaa, sekä helposti koneiden jäsentämää ja luotavaa. Ohjelmoijat käyttävät TOMLia selkeiden, hierarkkisten konfiguraatiotiedostojen kanssa projekteissa, joissa luettavuus on avainasemassa.

## Kuinka:
TOMLin lukemiseen ja manipulointiin Fish-skriptikielessä voit käyttää työkalua kuten `yj`, joka voi muuntaa TOMLin JSON-muotoon. Näin se tehdään:

```fish
# Asenna yj Fisherin kautta
fisher install jorgebucaran/yj

# Muunna TOML JSONiksi
echo 'title = "TOML Esimerkki"' | yj -tj

# Esimerkkituloste
{"title":"TOML Esimerkki"}
```

TOMLia kirjoittaessa prosessi käännetään ympäri:

```fish
# Muunna JSON TOMLksi
echo '{"title":"JSON Esimerkki"}' | yj -jt

# Esimerkkituloste
title = "JSON Esimerkki"
```

Raskaampiin tehtäviin harkitse omistettua TOML CLI-työkalua, kuten `toml-cli`.

```fish
# Asenna toml-cli
pip install toml-cli

# Aseta arvo TOML-tiedostoon
toml set pyproject.toml tool.poetry.version "1.1.4"

# Hae arvo TOML-tiedostosta
set version (toml get pyproject.toml tool.poetry.version)
echo $version
```

## Syväsukellus
TOML (Tom's Obvious, Minimal Language), jonka esitteli Tom Preston-Werner vuonna 2013, on samankaltainen kuin INI, mutta sillä on määritelty spesifikaatio ja datahierarkia. JSON ja YAML ovat päävaihtoehtoja, mutta niillä on omat kompromissinsa: JSON ei ole yhtä ihmisystävällinen, kun taas YAML on monimutkaisempi. TOMLin suunnittelu menestyy skenaarioissa, joissa konfiguraatiotiedostoja usein ylläpidetään käsin, tasapainottaen yksinkertaisuuden ja ilmaisuvoiman. Implementoinnin kannalta, TOML-jäsentäjiä on saatavilla useimmille ohjelmointikielille, mukaan lukien TomlBombadil Fishille, joka sopii suoraan skripteihisi.

## Katso Myös
- TOML virallinen spesifikaatio: https://toml.io
- `yj`, työkalu muuntaa TOML:n, JSON:n, YAML:n ja XML:n välillä: https://github.com/jorgebucaran/yj
- `toml-cli`, komentorivityökalu TOML:lle: https://github.com/sdispater/toml-cli
