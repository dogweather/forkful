---
title:                "Jobbe med TOML"
aliases:
- /no/java/working-with-toml/
date:                  2024-01-26T04:23:10.337980-07:00
model:                 gpt-4-0125-preview
simple_title:         "Jobbe med TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/working-with-toml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
TOML står for Toms Opplagte, Minimale Språk. Det er et data serialiseringsformat brukt for konfigurasjonsfiler. Programmerere bruker det fordi det er lett å lese, skrive, og kartlegges fint til en hashtabell.

## Hvordan:
Du trenger et TOML tolkingsbibliotek. Jeg anbefaler `toml4j`. Legg det til i prosjektet ditt slik:

```java
// Legg dette til i din build.gradle
dependencies {
    implementation 'com.moandjiezana.toml:toml4j:0.7.2'
}
```

Her er hvordan du leser en TOML-fil:

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
        
        System.out.println("Server IP: " + ip);
        System.out.println("Serverport: " + port);
    }
}
```

Eksempel på output:

```
Server IP: 192.168.1.1
Serverport: 80
```

## Dypdykk
Utviklet av GitHub medgrunnlegger Tom Preston-Werner, hadde TOML som mål å være enklere enn XML og mer spesifisert enn YAML. Dens siste versjon 1.0.0, utgitt i 2021, tilbyr et stabilt sett med funksjoner.

Alternativer som JSON eller YAML er også populære. JSON er flott for datautveksling. YAML er mer lesevennlig for komplekse konfigurasjoner. TOMLs styrke er dets direkthet og bruk i Rust-samfunnet.

Når det gjelder implementasjon, når du bruker TOML med Java, husk at tolkeren du velger har betydning. Utover `toml4j`, går noen for `jackson-dataformat-toml`. De vil hver ha sine nyanser, som feilhåndtering eller tolkingsytelse, så velg basert på prosjektets behov.

## Se Også
- TOML Spesifikasjon: https://toml.io/en/
- `toml4j` GitHub: https://github.com/mwanji/toml4j
- `jackson-dataformat-toml`: https://github.com/FasterXML/jackson-dataformats-text/tree/main/toml
