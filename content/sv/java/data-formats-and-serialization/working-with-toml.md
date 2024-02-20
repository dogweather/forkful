---
date: 2024-01-26 04:23:21.874473-07:00
description: "TOML st\xE5r f\xF6r Toms Uppebara, Minimala Spr\xE5k. Det \xE4r ett\
  \ data-serialiseringsformat som anv\xE4nds f\xF6r konfigurationsfiler. Programmerare\
  \ anv\xE4nder det eftersom\u2026"
lastmod: 2024-02-19 22:04:57.021236
model: gpt-4-0125-preview
summary: "TOML st\xE5r f\xF6r Toms Uppebara, Minimala Spr\xE5k. Det \xE4r ett data-serialiseringsformat\
  \ som anv\xE4nds f\xF6r konfigurationsfiler. Programmerare anv\xE4nder det eftersom\u2026"
title: Att arbeta med TOML
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
