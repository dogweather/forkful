---
title:                "Arbeiten mit JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/working-with-json.md"
---

{{< edit_this_page >}}

## Was & Warum?
JSON steht für JavaScript Object Notation und ist ein Format zum Austauschen von Daten zwischen Server und Client sowie zum Speichern von Textinformationen. Programmiere nutzen es, weil es leicht lesbar und in vielen Sprachen einfach zu verwenden ist, insbesondere in Web-Technologien.

## So geht's:
Um mit JSON in Java zu arbeiten, brauchst du eine Bibliothek wie `json-simple`. Hier ist ein kurzes Beispiel:

```java
import org.json.simple.JSONObject;

public class JsonBeispiel {
    public static void main(String[] args) {
        JSONObject obj = new JSONObject();
        
        obj.put("name", "Max Mustermann");
        obj.put("alter", 25);
        
        System.out.println(obj.toJSONString());
    }
}
```
Ausgabe:
```
{"name":"Max Mustermann","alter":25}
```

Zum Einlesen von JSON:

```java
import org.json.simple.parser.JSONParser;

public class JsonEinlesen {
    public static void main(String[] args) {
        JSONParser parser = new JSONParser();
        String jsonString = "{\"name\":\"Max Mustermann\",\"alter\":25}";
        
        try {
            JSONObject json = (JSONObject) parser.parse(jsonString);
            System.out.println(json.get("name"));
            System.out.println(json.get("alter"));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```
Ausgabe:
```
Max Mustermann
25
```

## Deep Dive
JSON existiert seit den frühen 2000er Jahren und wurde als Alternative zu XML entwickelt, da es kompakter und schneller zu verarbeiten ist. Alternativen zu `json-simple` sind Bibliotheken wie `Gson` von Google oder `Jackson`. Diese bieten oft komplexere Funktionen wie Datenbindung und Streaming. Wichtig ist, dass die JSON-Struktur und die Java-Objekte gut zusammenpassen. Reflection und Annotations in Java erleichtern oft die Arbeit mit komplexen JSON-Dokumenten.

## Siehe Auch:
- [json-simple GitHub](https://github.com/fangyidong/json-simple)
- [JSON.org](https://www.json.org/json-de.html)
- [Gson User Guide](https://github.com/google/gson/blob/master/UserGuide.md)
- [Jackson Project](https://github.com/FasterXML/jackson)
