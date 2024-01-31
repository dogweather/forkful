---
title:                "YAML-tiedostojen käsittely"
date:                  2024-01-19
simple_title:         "YAML-tiedostojen käsittely"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? | Mitä & Miksi?
YAML on dataformaatti, joka on helppo lukea ja kirjoittaa. Ohjelmoijat käyttävät YAMLia konfiguraatiotiedostoihin ja datan välittämiseen, koska se on selkeä ja yhteensopiva monien ohjelmointikielien kanssa.

## How to: | Miten:
Python-koodilla YAML-tiedoston käsittely onnistuu näin. Tarvitaan `pyyaml`-kirjasto, joka asennetaan komennolla `pip install pyyaml`.

```Python
import yaml

# Luetaan YAML-tiedosto
with open('esimerkki.yaml', 'r') as file:
    data = yaml.load(file, Loader=yaml.FullLoader)
    print(data)

# Kirjoitetaan YAML-tiedostoon
uusi_data = {'Osa': 'Ohjelmointi', 'Kieli': 'Python'}
with open('uusi_esimerkki.yaml', 'w') as file:
    yaml.dump(uusi_data, file)
```

## Deep Dive | Syväsukellus:
YAML (YAML Ain't Markup Language) julkaistiin 2001 ja on suunniteltu olemaan ihmisluettava ja yksinkertainen. Sen rakenne on JSON:in ja XML:n vaihtoehto. YAMLin käyttö Pythonissa perustuu `pyyaml`-kirjastoon, joka puolestaan käyttää LibYAML:ää, C-kirjastoa YAMLin parsi- ja luontitoimintoihin.

## See Also | Katso Myös:
- YAML-formaatin virallinen sivusto: https://yaml.org
- `pyyaml`-kirjaston dokumentaatio: https://pyyaml.org/wiki/PyYAMLDocumentation
- Pythonin viralliset ohjeet tehty helposti ymmärrettäviksi: https://docs.python.org/3/
