---
title:                "Kotlin: Att arbeta med yaml"
simple_title:         "Att arbeta med yaml"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför

Att arbeta med YAML är en viktig del av Kotlin-programmering. YAML är ett lättläst och språket vilket gör det till en utmärkt filformat för att hantera konfigurationer och data. Genom att lära sig YAML kan du enkelt läsa och skriva strukturerad data, vilket är oumbärligt för många utvecklingsprojekt.

## Hur man gör det

För att använda YAML i ditt Kotlin-projekt, behöver du först importera en extern bibliotek. Vanligtvis använder man SnakeYAML-biblioteket, så se till att lägga till det som en beroende i ditt `build.gradle`-fil.

```Kotlin
dependencies {
    implementation("org.yaml:snakeyaml:1.26")
}
```

När du har importerat biblioteket, kan du använda dess funktioner för att läsa och skriva YAML-filer. Här är ett enkelt exempel på hur du läser en YAML-fil och skriver ut dess innehåll:

```Kotlin
val yamlString = File("example.yaml").readText()
val yamlData = Yaml().load<Map<String, Any>>(yamlString)
println(yamlData)
```

Detta kodsnutt kommer att läsa en YAML-fil vid namn `example.yaml`, konvertera den till en `Map` struktur och skriva ut dess innehåll. En liknande process kan användas för att skriva YAML-filer genom att först skapa en `Map` med data och sedan konvertera den till en YAML-sträng och skriva till en fil. Det finns många fler funktioner i SnakeYAML-biblioteket, så se till att utforska dokumentationen för att lära dig mer om dess möjligheter.

## Djupdykning

YAML är en strukturerad dataformat som är baserat på nyckel-värde-par. Det finns dock ett antal olika sätt att strukturera en YAML-fil, så det är viktigt att förstå skillnaderna och välja rätt struktur för dina specifika behov. Här är en kort förklaring av de vanligaste strukturerna:

- Enkel struktur: En YAML-fil med en enda nyckel-värde-paret.
- Map-struktur: En YAML-fil med flera nyckel-värde-par som lagras i en `Map`.
- Lista-struktur: En YAML-fil med flera nyckel-värde-par som lagras i en `List`.
- YAML-dokument: En YAML-fil som innehåller flera separata YAML-dokument separerade av tre streck (`---`). Detta kan vara användbart när du vill ha flera liknande strukturerade data i en fil.

Det finns också många andra konfigurationsalternativ för YAML, såsom att ange datatyper, kommentarer och referenser. Se till att utforska dokumentationen för att lära dig mer om dessa funktioner.

## Se också

- [SnakeYAML-dokumentation](https://bitbucket.org/asomov/snakeyaml)
- [YAML.org](https://yaml.org/)