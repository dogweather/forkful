---
title:                "Manipulation de JSON"
html_title:           "Arduino: Manipulation de JSON"
simple_title:         "Manipulation de JSON"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
Travailler avec JSON, c'est manipuler des données structurées simples et flexibles. Les programmeurs utilisent JSON pour échanger des informations de manière lisible entre serveurs et applications web.

## How to:
### Parse JSON:
```java
import org.json.JSONObject;

public class Main {
    public static void main(String[] args) {
        String jsonData = "{\"name\":\"John\", \"age\":30}";
        JSONObject obj = new JSONObject(jsonData);
        System.out.println("Name: " + obj.getString("name"));
        System.out.println("Age: " + obj.getInt("age"));
    }
}
```
Sortie:
```
Name: John
Age: 30
```

### Create JSON:
```java
import org.json.JSONObject;

public class Main {
    public static void main(String[] args) {
        JSONObject obj = new JSONObject();
        obj.put("name", "John");
        obj.put("age", 30);
        System.out.println(obj.toString());
    }
}
```
Sortie:
```
{"name":"John","age":30}
```

## Deep Dive
JSON (JavaScript Object Notation) a été introduit au début des années 2000 comme une alternative à XML, plus simple à utiliser. Des bibliothèques comme `org.json`, `Jackson` et `Gson` facilitent la manipulation de JSON en Java. Le choix dépend de facteurs comme la performance et la facilité d'utilisation. Le JSON est léger et est souvent utilisé dans des applications RESTful pour les communications API.

## See Also
- Documentation officielle JSON: https://www.json.org/json-en.html
- Bibliothèque `org.json`: https://github.com/stleary/JSON-java
- Jackson Project: https://github.com/FasterXML/jackson
- Google Gson: https://github.com/google/gson
- Tutoriel JSON: https://www.w3schools.com/js/js_json_intro.asp
