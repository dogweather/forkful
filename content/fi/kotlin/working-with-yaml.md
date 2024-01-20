---
title:                "Työskentely yaml:n kanssa"
html_title:           "Kotlin: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
YAML on yleisesti käytetty tiedostomuoto, jota ohjelmoijat käyttävät tallentaakseen ja siirtääkseen tietoja. Se on erityisen hyödyllinen, kun kyseessä on monimutkaisten tietorakenteiden tallentaminen ja jakaminen, sillä se on helppolukuinen ja helppo muokata. Koodaajat käyttävät YAMLia parantaakseen tietojenhallinnan tehokkuutta ja ylläpidettävyyttä.

## Kuinka:
Kotlin-ohjelmoijat voivat käyttää sisäänrakennettua yaml-kirjastoa käsitelläkseen YAML-muotoisia tiedostoja. Alla on esimerkki siitä, kuinka voit lukea YAML-tiedoston ja tulostaa sen sisällön:

```Kotlin
import org.yaml.snakeyml.Yaml

fun main() {
    val yaml = Yaml()
    val file = File("file.yaml")
    val data = yaml.load(file)
    println(data)
}
```
**Tulos:**
```
{ name: John, age: 30, job: Developer }
```

## Syvemmälle:
YAML-kieli julkaistiin vuonna 2001 ja sen tavoitteena oli korvata XML-tiedostot. Se on kuitenkin erilainen kuin XML, sillä se on suunniteltu ihmisen luettavaksi ja muokattavaksi. Muita vaihtoehtoja YAML:lle ovat esimerkiksi JSON ja TOML. Yhteensopivuus YAML-kääntöluokkien ja json.org:n kanssa antaa mahdollisuuden helposti muuntaa YAML-tiedosto JSON-muotoon ja päinvastoin.

## Katso myös:
- [YAML virallinen sivusto](https://yaml.org/)