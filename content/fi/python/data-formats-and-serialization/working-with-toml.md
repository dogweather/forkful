---
date: 2024-01-26 04:25:35.149524-07:00
description: "TOML, lyhenne sanoista Tom's Obvious, Minimal Language, on tiedon serialisointiformaatti,\
  \ joka muistuttaa JSONia tai YAMLia, mutta pyrkii\u2026"
lastmod: '2024-03-11T00:14:30.094131-06:00'
model: gpt-4-0125-preview
summary: "TOML, lyhenne sanoista Tom's Obvious, Minimal Language, on tiedon serialisointiformaatti,\
  \ joka muistuttaa JSONia tai YAMLia, mutta pyrkii\u2026"
title: "Ty\xF6skentely TOML:n kanssa"
---

{{< edit_this_page >}}

## Mikä & Miksi?
TOML, lyhenne sanoista Tom's Obvious, Minimal Language, on tiedon serialisointiformaatti, joka muistuttaa JSONia tai YAMLia, mutta pyrkii yksinkertaisuuteen ja luettavuuteen. Ohjelmoijat käyttävät TOMLia konfiguraatiotiedostoihin, koska se on helppo kirjoittaa ja ymmärtää, ja se kartoittuu siististi ohjelmointikielten, kuten Pythonin, tietorakenteisiin.

## Kuinka:
Ennen sukeltamista sisään, asenna `toml`-paketti komennolla `pip install toml`. Käsitellään TOML-tiedosto:

```python
import toml

# Esimerkki TOML-sisällöstä merkkijonona
toml_string = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z # Ensiluokkaiset päivämäärät

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
"""

# Jäsennetään TOML-merkkijono
parsed_toml = toml.loads(toml_string)

# Tietojen käyttäminen
print(parsed_toml['owner']['name'])  # Tulostus: Tom Preston-Werner
print(parsed_toml['database']['ports'])  # Tulostus: [8001, 8001, 8002]
```

## Syväsukellus
TOML:n loi Tom Preston-Werner, yksi GitHubin perustajista, käyttäjäystävällisemmäksi konfiguraatiotiedostomuodoksi. Sen on suunniteltu määrittämään yksiselitteisesti hajautustaulukkoon ja olemaan helposti koneellisesti jäsentävissä.

Verrattuna JSONiin, TOML on luettavampi konfiguraatiotiedostoille ja tukee kommentteja. YAML, toinen vaihtoehto, voi olla tiiviimpi, mutta sen riippuvuus sisennyksestä ja hienovaraisista ongelmista, kuten välilehtien kieltämisestä, voi aiheuttaa kompastuskiviä.

Mitä tulee toteutuksen yksityiskohtiin, TOML-arvot ovat tyypitettyjä, joihin kuuluvat merkkijonot, kokonaisluvut, liukuluvut, totuusarvot, päivämäärät, taulukot ja taulut. Kaikki on kirjainkoosta riippuvaa. Lisäksi, TOML tukee monirivisiä merkkijonoja ja, viimeisimmässä versiossa, jopa sallii erityyppiset taulukot.

Python käyttää `toml`-kirjastoa, joka peilaa JSON- ja YAML-kirjastojen API:tä. Käytössäsi on `toml.load` ja `toml.loads` TOML:n lukemiseen tiedostosta tai merkkijonosta vastaavasti, sekä `toml.dump` ja `toml.dumps` sen kirjoittamiseen.

## Katso Myös
- Virallinen TOML GitHub-repositorio spekseille: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `toml` Python-kirjaston dokumentaatio: [pypi.org/project/toml/](https://pypi.org/project/toml/)
- Todellisen maailman esimerkkejä TOML:sta: Rustin paketinhallintajärjestelmän `cargo` tai Pythonin pakkaustyökalun `poetry` konfiguraatiotiedostot.
