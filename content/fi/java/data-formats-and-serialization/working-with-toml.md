---
title:                "Työskentely TOML:n kanssa"
aliases:
- /fi/java/working-with-toml.md
date:                  2024-01-26T04:22:55.134535-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely TOML:n kanssa"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/working-with-toml.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
TOML tarkoittaa Tom's Obvious, Minimal Language -kieltä. Se on datan serialisointiformaatti, jota käytetään konfiguraatiotiedostoissa. Ohjelmoijat käyttävät sitä, koska se on helppolukuinen, helppokirjoitettava ja kuvautuu hyvin hajautustauluun.

## Kuinka:
Tarvitset TOML-jäsennyskirjaston. Suosittelen `toml4j`:tä. Lisää se projektiisi näin:

```java
// Lisää tämä build.gradle-tiedostoosi
dependencies {
    implementation 'com.moandjiezana.toml:toml4j:0.7.2'
}
```

Näin jäsennät TOML-tiedoston:

```java
import com.moandjiezana.toml.Toml;

public class TomlEsimerkki {
    public static void main(String[] args) {
        Toml toml = new Toml().read("""
            [server]
            ip = "192.168.1.1"
            port = 80
            """);

        String ip = toml.getString("server.ip");
        Integer port = toml.getLong("server.port").intValue();
        
        System.out.println("Palvelimen IP: " + ip);
        System.out.println("Palvelimen portti: " + port);
    }
}
```

Esimerkkilähtö:

```
Palvelimen IP: 192.168.1.1
Palvelimen portti: 80
```

## Syväsukellus
GitHubin perustajan Tom Preston-Wernerin kehittämä TOML pyrki olemaan yksinkertaisempi kuin XML ja määritellympi kuin YAML. Sen viimeisin versio 1.0.0, julkaistu vuonna 2021, tarjoaa vakavan ominaisuuskokonaisuuden.

Vaihtoehtoja kuten JSON tai YAML ovat myös suosittuja. JSON on loistava datan vaihtoon. YAML on ihmislukuisempi monimutkaisiin konfiointitiedostoihin. TOML:n vahvuus on sen suoraviivaisuus ja sen käyttö Rust-yhteisössä.

Toteutusta ajatellen, kun käytät TOML:ää Javan kanssa, pidä mielessä, että valitsemasi jäsennin on merkityksellinen. `toml4j`n lisäksi jotkut valitsevat `jackson-dataformat-toml`n. Niillä on kullakin omat vivahteensa, kuten virheenkäsittely tai jäsennysnopeus, joten valitse projektisi tarpeiden mukaan.

## Katso Myös
- TOML-määritys: https://toml.io/fi/
- `toml4j` GitHub: https://github.com/mwanji/toml4j
- `jackson-dataformat-toml`: https://github.com/FasterXML/jackson-dataformats-text/tree/main/toml
