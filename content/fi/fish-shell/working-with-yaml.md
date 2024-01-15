---
title:                "Työskentely yaml:n kanssa"
html_title:           "Fish Shell: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi: 

Miksi työskentelisit YAML:n kanssa? 
YAML on lyhenne sanoista "YAML Ain't Markup Language" ja se on yksi suosituimmista tiedostomuodoista tänä päivänä. Se on helppo lukea ja kirjoittaa, joten se on erinomainen valinta tiedostojen tallentamiseen ja siirtoon.

## Miten:

Fish Shell tekee YAML:n käsittelystä yksinkertaista ja helppoa. Lue alla olevat esimerkit nähdäksesi miten voit käyttää Fish Shellin toimintoja YAML-tiedostojen kanssa ja millainen tulos niistä saadaan.

```Fish Shell
# Asenna YAML-kirjasto
brew install libyaml

# Lue YAML-tiedosto
set data (yq r tiedosto.yaml)

# Hae tietty tieto YAML-tiedostosta
set nimi (yq r tiedosto.yaml name)

# Lisää uusi tieto YAML-tiedostoon
set uusi_data (yq w -i tiedosto.yaml uusi_tieto "uusi arvo")

# Poista tieto YAML-tiedostosta
set poistettu_tieto (yq d tiedosto.yaml poistettu_tieto)
```

Merkkijonon voi myös parsia YAML-muotoon käyttämällä `yq read` -komentoa:

```Fish Shell
set data "nimi: Henkilo
ika: 30"
set parsed (yq read -R "$data")
echo "Nimi: $parsed.nimi, Ikä: $parsed.ika"
```

```
Output:
Nimi: Henkilo, Ikä: 30
```

## Syvemmällä:

Fish Shellilla on monia toimintoja, jotka tekevät YAML:n käsittelystä sujuvaa ja tehokasta. Voit käyttää `yq` -komentoa lukemiseen, kirjoittamiseen ja muokkaamiseen sekä `[` -operaattoria tietojen hakuun. Voit myös käyttää `yaml eval` -komentoa suorittaaksesi YAML-muotoisen merkkijonon ja `yaml format` -komentoa muotoillessasi YAML-tiedostoa.

Fish Shell tarjoaa myös apuna `--pretty` parametrin, joka helpottaa YAML-formaatin luettavuutta ja tiedostojen vertailua.

## Katso myös:

- [Fish Shellin virallinen sivusto](https://fishshell.com/)
- [Fish Shellin dokumentaatio YAML-toiminnoista](https://fishshell.com/docs/current/cmds/yq.html)
- [YAML-spesifikaatiot](https://yaml.org/spec/)