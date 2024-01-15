---
title:                "Att arbeta med yaml"
html_title:           "Java: Att arbeta med yaml"
simple_title:         "Att arbeta med yaml"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför

Om du är en Java-utvecklare som jobbar med konfigurationsfiler, vill lära dig mer om YAML-formatet eller bara behöver en enkel och lättanvändbar syntax för att hantera datastrukturer, så är detta artikeln för dig! YAML är ett populärt format för att läsa och skriva datastrukturer, och medan det finns andra alternativ som JSON och XML, så skiljer sig YAML genom sin läsbara syntax och stöd för kommentarer.

## Hur man gör det

För att använda YAML i din Java-kod, behöver du först lägga till en beroende i ditt projekt. Du kan antingen använda en byggverktyg som Maven eller lägga till beroendet manuellt. För Maven-projekt, lägg till följande kod i din pom.xml fil:

```
<dependency>
  <groupId>org.yaml</groupId>
  <artifactId>snakeyaml</artifactId>
  <version>1.27</version>
</dependency>
```

Med beroendet tillagt, kan du nu börja använda YAML i din Java-kod. Här är ett exempel som läser in en YAML-fil och sparar datan i en Map:

```Java
// Importera nödvändiga bibliotek
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.util.Map;

// Skapa en inläsningsström
InputStream inputStream = this.getClass().getClassLoader().getResourceAsStream("config.yaml");

// Använd Yaml-biblioteket för att läsa in datan
Yaml yaml = new Yaml();
Map<String, String> data = yaml.load(inputStream);

// Nu kan du jobba med datan som en vanlig Map i din kod
System.out.println(data.get("server"));
```

### Exempel på YAML-fil

```
server: localhost
port: 8080
database:
  name: testdb
  username: admin
  password: secret
```

### Exempel på utmatning

```
localhost
```

## Utforska djupare

YAML-formatet är mer än bara en enkel syntax för att lagra data, det har också stöd för mer avancerade funktioner som jag listar här:

- Möjlighet att inkludera andra filer med "mergade" data
- Variabler som kan återanvändas i hela filen
- Kommentarer för att förklara datatypen eller funktionaliteten hos dina datastrukturer
- Användning av specialtecken för att ange datatyper som strängar, booleans och datum
- Möjlighet att lägga till metadata som t.ex. version och skapare av filen

## Se även

- [SnakeYAML dokumentation](https://bitbucket.org/asomov/snakeyaml/wiki/Documentation)
- [Java YAML-biblioteket på GitHub](https://github.com/BizBoard/java-yaml)