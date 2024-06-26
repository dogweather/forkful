---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:48.927001-07:00
description: "Kuinka: Kotlinissa stderr:iin kirjoittaminen voidaan saavuttaa k\xE4\
  ytt\xE4m\xE4ll\xE4 `System.err.println()`. T\xE4m\xE4 metodi on samankaltainen kuin\u2026"
lastmod: '2024-03-13T22:44:56.547740-06:00'
model: gpt-4-0125-preview
summary: "Kotlinissa stderr:iin kirjoittaminen voidaan saavuttaa k\xE4ytt\xE4m\xE4\
  ll\xE4 `System.err.println()`."
title: Kirjoittaminen standardivirheeseen
weight: 25
---

## Kuinka:
Kotlinissa stderr:iin kirjoittaminen voidaan saavuttaa käyttämällä `System.err.println()`. Tämä metodi on samankaltainen kuin `System.out.println()`, mutta ohjaa tulosteen standardivirhevirtaan eikä standarditulostevirtaan.

```kotlin
fun main() {
    System.err.println("Tämä on virheviesti!")
}
```

Esimerkkituloste:
```
Tämä on virheviesti!
```

Rakenteellisempien tai monimutkaisempien sovellusten kohdalla, erityisesti kun käytössä ovat lokituskehykset kuten Logback tai SLF4J, voit konfiguroida lokittajat kirjoittamaan tiettyjen lokitasojen (esim. VIRHE) viestejä stderr:iin.

Käyttäen SLF4J:ta Logbackin kanssa:

1. Ensiksi, lisää SLF4J API ja Logbackin toteutus `build.gradle`-tiedostoosi:

```groovy
dependencies {
    implementation 'org.slf4j:slf4j-api:1.7.30'
    implementation 'ch.qos.logback:logback-classic:1.2.3'
}
```

2. Seuraavaksi, konfiguroi Logback (tiedostossa `src/main/resources/logback.xml`) ohjaamaan virhetason viestit stderr:iin:

```xml
<configuration>
    <appender name="STDERR" class="ch.qos.logback.core.ConsoleAppender">
        <target>System.err</target>
        <encoder>
            <pattern>%d{yyyy-MM-dd HH:mm:ss} [%thread] %-5level %logger{36} - %msg%n</pattern>
        </encoder>
    </appender>
    
    <root level="error">
        <appender-ref ref="STDERR" />
    </root>
</configuration>
```

3. Sitten, käytä SLF4J:ta Kotlin-koodissasi logitaksesi virheviestejä:

```kotlin
import org.slf4j.LoggerFactory

fun main() {
    val logger = LoggerFactory.getLogger("ExampleLogger")
    logger.error("Tämä on virhelokitietue!")
}
```

Esimerkkituloste (stderr:iin):
```
2023-04-01 12:34:56 [main] VIRHE ExampleLogger - Tämä on virhelokitietue!
```
