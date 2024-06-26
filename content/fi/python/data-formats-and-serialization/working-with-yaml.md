---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:37.492190-07:00
description: "Kuinka: YAMLin lukeminen ja kirjoittaminen Pythonissa tyypillisesti\
  \ sis\xE4lt\xE4\xE4 kolmannen osapuolen kirjaston k\xE4yt\xF6n, `PyYAML` ollessa\
  \ suosituin.\u2026"
lastmod: '2024-03-13T22:44:56.163860-06:00'
model: gpt-4-0125-preview
summary: "YAMLin lukeminen ja kirjoittaminen Pythonissa tyypillisesti sis\xE4lt\xE4\
  \xE4 kolmannen osapuolen kirjaston k\xE4yt\xF6n, `PyYAML` ollessa suosituin."
title: "Ty\xF6skentely YAML:n kanssa"
weight: 41
---

## Kuinka:
YAMLin lukeminen ja kirjoittaminen Pythonissa tyypillisesti sisältää kolmannen osapuolen kirjaston käytön, `PyYAML` ollessa suosituin. Aloittaaksesi sinun täytyy asentaa PyYAML suorittamalla `pip install PyYAML`.

**Esimerkki: Kirjoittaminen YAML-tiedostoon**

```python
import yaml

data = {'lista': [1, 42, 3.141, 1337, 'apua', u'€'],
        'merkkijono': 'boo!',
        'toinen sanakirja': {'foo': 'bar', 'avain': 'arvo', 'vastaus': 42}}

with open('esimerkki.yaml', 'w') as f:
    yaml.dump(data, f, default_flow_style=False)

# Tämä luo `esimerkki.yaml` tiedoston datalla rakenteistettuna YAML-muotoon.
```

**Esimerkki: Lukeminen YAML-tiedostosta**

```python
import yaml

with open('esimerkki.yaml', 'r') as f:
    datan_ladattu = yaml.safe_load(f)

print(datan_ladattu)

# Tuloste: 
# {'lista': [1, 42, 3.141, 1337, 'apua', '€'],
#  'merkkijono': 'boo!',
#  'toinen sanakirja': {'foo': 'bar', 'avain': 'arvo', 'vastaus': 42}}
```

**YAMLin käyttö konfiguraatiossa**

Monet ohjelmoijat käyttävät YAMLia sovellusten konfiguraatioiden hallintaan. Tässä on esimerkki siitä, kuinka yksi voisi rakentaa konfiguraatiotiedoston ja lukea sen:

config.yaml:
```yaml
tietokanta:
  isäntä: localhost
  portti: 5432
  käyttäjätunnus: admin
  salasana: salaisuus
```

Konfiguraatiotiedoston lukeminen Pythonissa:
```python
import yaml

with open('config.yaml', 'r') as f:
    config = yaml.safe_load(f)

print(config['tietokanta']['isäntä'])  # Tuloste: localhost
```

**Monimutkaisten rakenteiden käsittely**

Monimutkaisia rakenteita varten PyYAML mahdollistaa mukautettujen Python-objektien määrittelyn. Kuitenkin, varmista turvallisuuden noudattaminen käyttämällä `safe_load` välttääksesi mielivaltaisten funktioiden tai objektien suorittamisen.

```python
import yaml

# Määritellään Python-objekti
class Esimerkki:
    def __init__(self, arvo):
        self.arvo = arvo

# Mukautettu konstruktori
def konstruktori_esimerkki(loader, node):
    arvo = loader.construct_scalar(node)
    return Esimerkki(arvo)

# Lisää konstruktori tagille "!esimerkki"
yaml.add_constructor('!esimerkki', konstruktori_esimerkki)

yaml_str = "!esimerkki 'data'"
ladattu = yaml.load(yaml_str, Loader=yaml.FullLoader)

print(ladattu.arvo)  # Tuloste: data
```

Tässä katkelmassa `!esimerkki` on mukautettu tagi, jota käytetään instansioimaan `Esimerkki` objekti arvolla 'data' YAML-merkkijonosta. Mukautetut laturit kuten tämä laajentavat PyYAMLin joustavuutta, mahdollistaen monimutkaisempien datarakenteiden ja tyyppien käsittelyn.
