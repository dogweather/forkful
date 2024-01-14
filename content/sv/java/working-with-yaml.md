---
title:                "Java: Att arbeta med yaml"
simple_title:         "Att arbeta med yaml"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför?

YAML är en populär och lättläst filformat för datastrukturer inom programmering. Att kunna arbeta med YAML i Java ger dig möjlighet att lättare hantera och manipulera data som används i ditt program. Det kan också underlätta kommunikationen mellan ditt program och andra system.

## Så här gör du:

För att kunna arbeta med YAML i Java behöver du ett externt bibliotek, såsom SnakeYAML. Nedan följer en enkel kod för att läsa in en YAML-fil och skriva ut dess innehåll:

```Java
import org.yaml.snakeyaml.Yaml;

public class Main {

    public static void main(String[] args) {
        // Skapa en instans av Yaml
        Yaml yaml = new Yaml();

        // Läs in YAML-filen och spara data i en Map
        Map<String, Object> data = yaml.load("sample.yaml");

        // Skriv ut innehållet på konsolen
        System.out.println(data.toString());
    }
}
```

Output:
```Java
{namn=Sara, ålder=30}
```

## Djupdykning:

YAML består av nyckel-värde-par som är enkla att läsa och skriva i jämförelse med andra dataformat. Det är också en utmärkt format för konfigurationsfiler, vilket gör det populärt inom systemadministration och DevOps. Med hjälp av biblioteket SnakeYAML kan du även analysera och manipulera YAML-filer på ett enkelt sätt i ditt Java-program.

## Se även:

- [SnakeYAML biblioteket](https://bitbucket.org/asomov/snakeyaml)
- [YAML.org](https://yaml.org/)
- [Officiell SnakeYAML dokumentation](https://bitbucket.org/asomov/snakeyaml/wiki/Documentation)