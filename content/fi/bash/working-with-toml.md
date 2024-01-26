---
title:                "Työskentely TOML:n kanssa"
date:                  2024-01-26T04:18:49.103858-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely TOML:n kanssa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/working-with-toml.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
TOML, lyhenne sanoista Tom's Obvious, Minimal Language, on datan serialisointiformaatti. Ohjelmoijat diggaavat sitä sen yksinkertaisuuden ja luettavuuden vuoksi; se on mahtava config-tiedostoille, samanlaisia fiiliksiä kuin YAML mutta vähemmän kömpelö kuin JSON ihmisen näkökulmasta.

## Miten:
Aloita asentamalla `toml-cli`, jotta voit leikkiä TOML:lla Bashissa. Kätevä lukiessa tai muokatessa TOML-tiedostoja lennosta.

```Bash
# Asenna toml-cli, meidän pieni apurimme TOML-tehtäviin
pip install toml-cli

# Kuvittele, että sinulla on TOML-tiedosto, 'config.toml'
echo -e 'title = "TOML Demo"\n\n[owner]\nname = "Tom"\ndob = 1979-05-27T07:32:00Z' > config.toml

# Lue arvo
toml get config.toml owner.name
# Tuloste: Tom

# Aseta arvo
toml set config.toml 'owner.dob' '2000-01-01T00:00:00Z'
# Pro-vinkki: Käytä lainausmerkkejä avaimiin, joissa on pisteitä tai omituisia merkkejä!
```

## Syväsukellus
Syntyisin JSONin ihmistä hankaloittavista esteistä, TOML putkahti esiin vuonna 2013. Tom Preston-Werner, GitHubin perustajaosaaja, halusi jotain superluettavaa. YAML ja INI olivat vaihtoehtoja, mutta TOML on kuin parasta molemmista.

Shebang, sinulla on sisäkkäisiä tietoja ja taulukoita, miinus YAML:n jalka-aseet ja JSONin aaltosulkeet. TOML on nyt mennyt-to configissa Rustin Cargolle, mikä puhuu sen noususta dev-maailmassa. Sitä ohjaa speksi, pitäen asiat tiukkoina ja hyvin määriteltyinä. Saat kaappaajia lähes mihin tahansa kieleen, mikä tekee siitä laajalti omaksuttavan.

## Katso Myös
- Virallinen TOML GitHub Repo: https://github.com/toml-lang/toml
- toml-cli PyPI:ssä: https://pypi.org/project/toml-cli/
- Data-serialisointiformaattien vertailu: https://fi.wikipedia.org/wiki/Data-serialisointiformaattien_vertailu