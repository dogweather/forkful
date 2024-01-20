---
title:                "YAML-tiedostojen käsittely"
html_title:           "Arduino: YAML-tiedostojen käsittely"
simple_title:         "YAML-tiedostojen käsittely"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä ja Miksi?)
YAML on datankuvauskieli konfiguraatioihin ja viestintään, joka on helppo lukea sekä koneille että ihmisille. Ohjelmoijat käyttävät YAMLia, koska se on yksinkertainen, laajalti tuettu ja tiiviisti integroitavissa erilaisiin sovelluksiin ja automaatioprosesseihin.

## How to: (Kuinka tehdä:)
```Bash
# YAML-tiedoston lukeminen komentosarjassa
apt update
apt install -y python3-pip
pip3 install pyyaml

# Luo yksinkertainen esimerkki YAML-tiedosto
echo 'tervehdys: hei maailma' > esimerkki.yaml

# Lue ja tulosta YAML-tiedoton sisältö
python3 -c 'import yaml; print(yaml.safe_load(open("esimerkki.yaml")))'

# Tulosteen pitäisi näyttää jotakin tältä:
# {'tervehdys': 'hei maailma'}
```

## Deep Dive (Syväsukellus)
YAML, lyhenne sanoista "YAML Ain't Markup Language" (aiemmin "Yet Another Markup Language"), on luotu vuonna 2001 helpottamaan konfiguraatioiden hallintaa ja tiedonjakoa eri ohjelmointikielissä ja -ympäristöissä. Vaihtoehtoja YAMLille ovat JSON ja XML, jotka ovat myös suosittuja datankuvauskieliä. YAML erottuu legibiliteetinsä ja pyrkimyksen vähämerkkiseen syntaksiin. Ohjelmakirjastot eri ohjelmointikielissä tarjoavat YAML-tiedostojen käsittelyn, ja se toimii usein konfiguraatioissa, kuten Dockerissa ja Kubernetesissa.

## See Also (Katso Myös)
- YAML: [https://yaml.org](https://yaml.org)
- YAML-syntaksi: [https://yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)
- YAML ja Python: [https://pyyaml.org/wiki/PyYAMLDocumentation](https://pyyaml.org/wiki/PyYAMLDocumentation)
- Bash-skriptausopas: [https://www.gnu.org/software/bash/manual/](https://www.gnu.org/software/bash/manual/)