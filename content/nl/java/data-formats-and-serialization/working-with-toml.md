---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:55.267511-07:00
description: 'Hoe te: Je hebt een TOML-verwerkingsbibliotheek nodig. Ik raad `toml4j`
  aan. Voeg dit toe aan je project als volgt.'
lastmod: '2024-03-13T22:44:50.705794-06:00'
model: gpt-4-0125-preview
summary: Je hebt een TOML-verwerkingsbibliotheek nodig.
title: Werken met TOML
weight: 39
---

## Hoe te:
Je hebt een TOML-verwerkingsbibliotheek nodig. Ik raad `toml4j` aan. Voeg dit toe aan je project als volgt:

```java
// Voeg dit toe aan je build.gradle
dependencies {
    implementation 'com.moandjiezana.toml:toml4j:0.7.2'
}
```

Hier is hoe je een TOML-bestand verwerkt:

```java
import com.moandjiezana.toml.Toml;

public class TomlExample {
    public static void main(String[] args) {
        Toml toml = new Toml().read("""
            [server]
            ip = "192.168.1.1"
            poort = 80
            """);

        String ip = toml.getString("server.ip");
        Integer poort = toml.getLong("server.port").intValue();
        
        System.out.println("Server IP: " + ip);
        System.out.println("Server Poort: " + poort);
    }
}
```

Voorbeeld van uitvoer:

```
Server IP: 192.168.1.1
Server Poort: 80
```

## Diepgaande Duik
Ontwikkeld door GitHub medeoprichter Tom Preston-Werner, was TOML gericht op eenvoudiger te zijn dan XML en meer gespecificeerd dan YAML. De laatste versie 1.0.0, uitgebracht in 2021, biedt een stabiele set functies.

Alternatieven zoals JSON of YAML zijn ook populair. JSON is geweldig voor gegevensuitwisseling. YAML is leesbaarder voor complexe configuraties. De kracht van TOML ligt in zijn eenvoud en het gebruik in de Rust-community.

Wat betreft de implementatie, wanneer je TOML met Java gebruikt, houd er dan rekening mee dat de parser die je kiest belangrijk is. Naast `toml4j`, kiezen sommigen voor `jackson-dataformat-toml`. Ze zullen elk nuances hebben, zoals foutafhandeling of verwerkingsprestaties, dus kies op basis van de behoeften van je project.

## Zie Ook
- TOML-specificatie: https://toml.io/en/
- `toml4j` GitHub: https://github.com/mwanji/toml4j
- `jackson-dataformat-toml`: https://github.com/FasterXML/jackson-dataformats-text/tree/main/toml
