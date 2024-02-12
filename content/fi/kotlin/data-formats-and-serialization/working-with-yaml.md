---
title:                "Työskentely YAML:n kanssa"
date:                  2024-02-03T19:26:03.149326-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely YAML:n kanssa"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?
YAML, joka tarkoittaa "YAML Ain't Markup Language", on erittäin luettavissa oleva datan serialisointimuoto, jota käytetään usein asetustiedostoissa, datan tallennuksessa ja prosessien välisessä viestinnässä. Ohjelmoijat työskentelevät usein YAML:n kanssa hallitakseen asetuksia ja määrityksiä rakenteellisella mutta yksinkertaisella tavalla, hyötyen sen selkeydestä ja yksinkertaisuudesta verrattuna JSON:iin tai XML:ään, kun luettavuus on tärkeää.

## Kuinka:
Kotlin ei tue sisäänrakennettuna YAML:n jäsennystä ja serialisointia, mutta voit hyödyntää suosittuja kolmannen osapuolen kirjastoja, kuten `snakeyaml` (yleiseen YAML-jäsennykseen) ja `kotlinx.serialization` (YAML-muoto laajennuksena) työskennelläksesi YAML-tiedostojen kanssa.

### Käyttämällä `snakeyaml`
**Riippuvuus:**
```kotlin
implementation 'org.yaml:snakeyaml:1.30'
```

**Lue YAML:**
```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.FileInputStream

fun readYaml(filePath: String) {
    val yaml = Yaml()
    val inputStream = FileInputStream(filePath)
    val data = yaml.load<Map<String, Any>>(inputStream)

    println(data)
}

// Esimerkkitapaus
fun main() {
    readYaml("config.yaml")
}
```
**Esimerkki `config.yaml`:**
```yaml
database:
  host: localhost
  port: 5432
```
**Esimerkkituloste:**
```
{database={host=localhost, port=5432}}
```
### Käyttämällä `kotlinx.serialization` YAML:n kanssa
Varmista ensin, että sinulla on `kotlinx-serialization`-kirjasto sopivalla YAML-tukikirjastolla (jos saatavilla, koska `kotlinx.serialization` suuntautuu ensisijaisesti JSON:iin ja muihin suoraan tuettuihin formaatteihin).

**Riippuvuus:**
```kotlin
// JSON:lle (havainnollistava, tarkista YAML-tuki tai vaihtoehtoiset kirjastot)
implementation 'org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.2'
```

**Määrittele serialisoituva data-luokka:**
```kotlin
import kotlinx.serialization.Serializable

@Serializable
data class Config(
    val database: Database
)

@Serializable
data class Database(
    val host: String,
    val port: Int
)
```

Valitettavasti kirjoitushetkellä suora YAML-tuki `kotlinx.serialization`-kirjastossa saattaa olla rajallinen tai kehittyvä. Saatat joutua käyttämään välimuotoa (kuten muuntaa YAML JSON:ksi `snakeyaml`-kirjastolla ja sitten jäsentää JSON `kotlinx.serialization`-kirjastolla) tai etsiä yhteisön vetämiä YAML-serialisointiprojekteja, jotka ovat yhteensopivia `kotlinx.serialization`-kirjaston kanssa.

JSONille koodi näyttäisi tältä:
```kotlin
import kotlinx.serialization.json.Json
import kotlinx.serialization.decodeFromString

fun main() {
    val jsonText = """
    {
        "database": {
            "host": "localhost",
            "port": 5432
        }
    }
    """.trimIndent()
    
    val config = Json.decodeFromString<Config>(jsonText)
    println(config)
}
```

Koska Kotlin ja sen ekosysteemi jatkavat kehittymistään, pidä silmällä virallista dokumentaatiota ja yhteisöresursseja saadaksesi viimeisimmät tiedot YAML-tuesta ja kirjastoista.
