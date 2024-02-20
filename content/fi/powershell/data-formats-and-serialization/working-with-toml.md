---
date: 2024-01-26 04:25:00.336120-07:00
description: "TOML, lyhenne sanoista Tom's Obvious, Minimal Language, on datan serialisointiformaatti,\
  \ joka on helppo lukea sen selke\xE4n semantiikan ansiosta.\u2026"
lastmod: 2024-02-19 22:05:15.701726
model: gpt-4-0125-preview
summary: "TOML, lyhenne sanoista Tom's Obvious, Minimal Language, on datan serialisointiformaatti,\
  \ joka on helppo lukea sen selke\xE4n semantiikan ansiosta.\u2026"
title: "Ty\xF6skentely TOML:n kanssa"
---

{{< edit_this_page >}}

## Mikä ja miksi?

TOML, lyhenne sanoista Tom's Obvious, Minimal Language, on datan serialisointiformaatti, joka on helppo lukea sen selkeän semantiikan ansiosta. Ohjelmoijat käyttävät sitä konfiguraatiotiedostoihin, koska se löytää tasapainon ihmisen luettavuuden ja koneen ystävällisyyden välillä.

## Miten:

PowerShellissä ei ole natiivia cmdlet-komentoa TOML:n jäsennykseen. Tyypillisesti käyttäisit moduulia tai muunnat TOML:n JSON:ksi työkalulla, kuten `toml-to-json`, jos haluat työskennellä PowerShellin kanssa. Näin tekisit sen kuvitteellisella moduulilla `PowerShellTOML`:

```PowerShell
# Ensin asenna moduuli (kuvitteellinen, demonstraatiovarten)
Install-Module PowerShellTOML

# Tuo TOML-tiedosto
$config = Import-TomlConfig -Path './config.toml'

# Arvon käyttö
Write-Output $config.database.server

# Esimerkki TOML-sisällöstä 'config.toml'-tiedostossa:
# [database]
# server = "192.168.1.1"
# ports = [ 8001, 8001, 8002 ]
# connection_max = 5000

# Esimerkki tulosteesta:
# 192.168.1.1
```

## Syväsukellus

TOML:n loi Tom Preston-Werner, GitHubin perustajaosakas, yksinkertaisena vaihtoehtona XML:lle ja YAML:lle konfiguraatiotiedostoissa. Sen ensimmäinen versio ilmestyi vuonna 2013. TOML on verrattavissa JSONiin, mutta suunniteltu olemaan ihmisystävällisempi, mikä tekee siitä hyvän valinnan konfiguraatioille, joita ihmiset ylläpitävät. Vaihtoehtoja ovat YAML, JSON ja XML.

Toteutuksen kannalta PowerShell-moduuli TOML:lle olisi tyypillisesti kääre TOML-kirjastolle, joka on kirjoitettu suorituskykyisemmällä kielellä, kuten C#. PowerShellillä ei ole sisäänrakennettua tukea TOML:lle, minkä vuoksi tällainen moduuli on tarpeellinen, jotta TOML-formaattiin voisi liittyä kätevästi.

## Katso myös

- TOML-standardi: https://toml.io/en/
- GitHub-repositorio `toml` PowerShell-moduulille (jos olemassa lukuhetkellä): https://github.com/powershell/PowerShellTOML
- Johdatus TOML:iin: https://github.com/toml-lang/toml
- Vertailu datan serialisointiformaatteihin: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
