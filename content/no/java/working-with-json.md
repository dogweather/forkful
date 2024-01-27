---
title:                "Arbeid med JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med JSON"
simple_title:         "Arbeid med JSON"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/working-with-json.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
JSON forvandler data til tekst som kan lagres og sendes mellom servere og klienter. Programmerere bruker det fordi det er lett å lese og skrive, samt lett å parse og generere av maskiner.

## Hvordan:
```java
import org.json.JSONObject;

public class Main {
    public static void main(String[] args) {
        // Oppretter en JSON objekt
        JSONObject obj = new JSONObject();
        obj.put("navn", "Ola Nordmann");
        obj.put("alder", 30);
        obj.put("erProgrammerer", true);

        // Skriver ut det som en streng
        System.out.println(obj.toString());

        // Parser en JSON streng til et objekt
        String jsonData = "{\"navn\":\"Kari Nordmann\",\"alder\":28,\"erProgrammerer\":false}";
        JSONObject nyObj = new JSONObject(jsonData);
        System.out.println(nyObj.toString());
    }
}
```
Sample output:
```
{"navn":"Ola Nordmann","erProgrammerer":true,"alder":30}
{"navn":"Kari Nordmann","erProgrammerer":false,"alder":28}
```

## Deep Dive
JSON (JavaScript Object Notation) ble designet tidlig på 2000-tallet og ble raskt web-utvikleres favoritt for å overføre data, takket være dets enkelhet sammenlignet med XML. Alternativer inkluderer XML og YAML, men JSON vinner ofte på grunn av støtte i JavaScript og de fleste andre programmeringsspråk. Java-biblioteker som Jackson og Gson kan også hjelpe med å implementere JSON i større prosjekter.

## See Also
- Offisiell JSON-nettside: [json.org](https://json.org)
- Jackson: [GitHub - FasterXML/jackson](https://github.com/FasterXML/jackson)
- Gson: [GitHub - google/gson](https://github.com/google/gson)
- JSON i Java [tutorial](https://www.oracle.com/technical-resources/articles/java/json.html) fra Oracle.
