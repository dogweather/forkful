---
title:                "YAML-tiedostojen käsittely"
date:                  2024-01-19
simple_title:         "YAML-tiedostojen käsittely"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
YAML on ihmisen luettava dataseriaaliformaatti, jota käytetään konfiguraatiotiedostoihin. Ohjelmoijat suosivat sitä sen selkeyden ja helpon parsinnan vuoksi.

## How to: (Kuinka tehdä:)
Kotlinin avulla voit käsitellä YAML-tiedostoja käyttämällä kirjastoja kuten 'snakeyaml'. Tässä yksinkertainen esimerkki:

```Kotlin
import org.yaml.snakeyaml.Yaml
import java.io.File

fun main() {
    val yaml = Yaml()
    val inputStream = File("config.yaml").inputStream()
    val data = yaml.load<Map<String, Any>>(inputStream)
    
    println(data["key"]) // Olettaen, että 'config.yaml' sisältää 'key: value' -parin
}
```

Tuloste:

```
value
```

## Deep Dive (Syväsukellus)
YAML (YAML Ain't Markup Language) kehitettiin 2000-luvun alussa, ja se on ottanut vaikutteita kielistä kuten C, Python ja XML. Vaihtoehtoja YAML:lle ovat JSON ja XML. JSON on yksinkertaisempi, mutta ei niin luettava, kun taas XML on monipuolinen mutta monimutkainen. YAML käyttää sisennystä datarakenteiden esittämiseen ja se on altis välilyöntivirheille, joten kirjastojen käyttäminen on suositeltavaa.

## See Also (Katso Myös)
- YAML virallinen sivusto: [https://yaml.org](https://yaml.org)
- JSON ja XML vertailu: [JSON vs XML](https://www.w3schools.com/js/js_json_xml.asp)
