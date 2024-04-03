---
date: 2024-01-26 04:23:42.916722-07:00
description: "TOML tarkoittaa Tom's Obvious, Minimal Language -kielt\xE4. Sit\xE4\
  \ k\xE4ytet\xE4\xE4n konfiguraatiotiedostoihin, koska se on ihmisten luettavissa\
  \ ja kirjoitettavissa\u2026"
lastmod: '2024-03-13T22:44:56.554705-06:00'
model: gpt-4-0125-preview
summary: "TOML tarkoittaa Tom's Obvious, Minimal Language -kielt\xE4."
title: "Ty\xF6skentely TOML:n kanssa"
weight: 39
---

## Mikä & Miksi?
TOML tarkoittaa Tom's Obvious, Minimal Language -kieltä. Sitä käytetään konfiguraatiotiedostoihin, koska se on ihmisten luettavissa ja kirjoitettavissa helposti, mutta koneiden on myös helppo jäsentää sitä. Kehittäjät turvautuvat TOMLiin välttääkseen XML:n sekavuuden ja JSON:n kikkailut, kun he käsittelevät konfiguraatioita.

## Kuinka:
Kotlinin kanssa TOMLin käsittelyyn voisi käyttää kirjastoa, kuten `ktoml`. Aloitetaan lisäämällä riippuvuus `build.gradle.kts`-tiedostoosi:

```kotlin
dependencies {
    implementation("com.akuleshov7:ktoml:0.2.5")
}
```

Nyt käsitellään hieman TOMLia:

```kotlin
import com.akuleshov7.ktoml.file.TomlFileReader

fun main() {
    val tomlContent = TomlFileReader.readAndParseFile("config.toml")
    
    val databaseConfig = tomlContent.getTable("database")
    val host = databaseConfig.getString("host")
    val port = databaseConfig.getLong("port")

    println("Tietokannan isäntä: $host")
    println("Tietokannan portti: $port")
}
```

Olettaen, että `config.toml` näyttää tältä:

```toml
[database]
host = "localhost"
port = 5432
```

Esimerkkituloste olisi:

```
Tietokannan isäntä: localhost
Tietokannan portti: 5432
```

## Syväsukellus
TOML, jonka loi GitHubin perustaja Tom Preston-Werner vuonna 2013, pyrki olemaan yksinkertaisempi kuin YAML ja turvallisempi tyyppien suhteen kuin JSON. Se on saavuttanut suosiota, erityisesti Rustin `Cargo`- ja Gon moduulijärjestelmän kanssa. Vaihtoehtoja? YAML:ssä on enemmän ominaisuuksia, JSON kääntyy suoraan olioiksi monissa ohjelmointikielissä, ja aina on myös hyvä vanha XML. Toteutuksesta puheen ollen, ktoml on Apache 2.0 -lisenssin alainen puhdas Kotlin-kirjasto, eikä se vedä Java-kirjastoja mukanaan, tarjoten DSL:t sekä TOMLin kirjoittamiseen että lukemiseen.

## Katso Myös
- TOML GitHub: https://github.com/toml-lang/toml
- ktoml GitHub: https://github.com/akuleshov7/ktoml
- TOML vs. YAML vs. JSON: https://blog.logrocket.com/comparing-configuration-files-yaml-toml-json/
