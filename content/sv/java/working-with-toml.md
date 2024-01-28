---
title:                "Att arbeta med TOML"
date:                  2024-01-26T04:23:21.874473-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att arbeta med TOML"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/working-with-toml.md"
---

{{< edit_this_page >}}

## Vad och Varför?
TOML står för Toms Uppebara, Minimala Språk. Det är ett data-serialiseringsformat som används för konfigurationsfiler. Programmerare använder det eftersom det är lätt att läsa, skriva och kartlägger väl till en hashtabell.

## Hur man gör:
Du behöver ett TOML-parserbibliotek. Jag rekommenderar `toml4j`. Lägg till det i ditt projekt så här:

```java
// Lägg till detta i din build.gradle
dependencies {
    implementation 'com.moandjiezana.toml:toml4j:0.7.2'
}
```

Så här parserar du en TOML-fil:

```java
import com.moandjiezana.toml.Toml;

public class TomlExample {
    public static void main(String[] args) {
        Toml toml = new Toml().read("""
            [server]
            ip = "192.168.1.1"
            port = 80
            """);

        String ip = toml.getString("server.ip");
        Integer port = toml.getLong("server.port").intValue();
        
        System.out.println("Serverns IP: " + ip);
        System.out.println("Serverns Port: " + port);
    }
}
```

Exempel på utskrift:

```
Serverns IP: 192.168.1.1
Serverns Port: 80
```

## Fördjupning
Utvecklad av GitHub-medgrundaren Tom Preston-Werner, syftade TOML till att vara enklare än XML och mer specificerad än YAML. Dess senaste version 1.0.0, släppt 2021, erbjuder en stabil uppsättning funktioner.

Alternativ som JSON eller YAML är också populära. JSON är bra för datautbyte. YAML är mer läsligt för komplexa konfigurationer. TOMLs styrka är dess rättframhet och dess användning i Rust-gemenskapen.

När det gäller implementering, när du använder TOML med Java, kom ihåg att det parser du väljer har betydelse. Utöver `toml4j`, väljer vissa `jackson-dataformat-toml`. De kommer var och en ha nyanser, som felsökning eller parsingprestanda, så välj baserat på ditt projekts behov.

## Se även
- TOML-specifikation: https://toml.io/en/
- `toml4j` GitHub: https://github.com/mwanji/toml4j
- `jackson-dataformat-toml`: https://github.com/FasterXML/jackson-dataformats-text/tree/main/toml
