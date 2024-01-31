---
title:                "Arbeta med JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeta med JSON"
simple_title:         "Arbeta med JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON (JavaScript Object Notation) är textbaserat dataformat. Programmerare använder JSON för att lagra och utbyta enkel och maskinläsbar information mellan system - det är snabbt och språkoberoende.

## How to:
Java använder `java.json` biblioteket för att hantera JSON. För att läsa och skriva JSON behövs bibliotek som `Jackson` eller `Gson`.

**Läsa JSON:**
```java
import org.json.JSONObject;

public class JsonDemo {
    public static void main(String[] args) {
        String jsonData = "{\"name\":\"Anna\",\"age\":30}";
        JSONObject jsonObject = new JSONObject(jsonData);
        
        String name = jsonObject.getString("name");
        int age = jsonObject.getInt("age");
        
        System.out.println("Namn: " + name);
        System.out.println("Ålder: " + age);
    }
}
```
**Utdata:**
```
Namn: Anna
Ålder: 30
```

**Skriva JSON:**
```java
import org.json.JSONObject;

public class JsonDemo {
    public static void main(String[] args) {
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("name", "Erik");
        jsonObject.put("age", 25);
        
        String jsonData = jsonObject.toString();
        System.out.println(jsonData);
    }
}
```
**Utdata:**
```
{"name":"Erik","age":25}
```

## Deep Dive:
JSON introducerades 2001. Det har blivit ett populärt alternativ till XML för att det är mindre krångligt och snabbare att tolka. Alternativ till `java.json` inkluderar bibliotek som `Jackson` och `Gson`. Jackson är snabbare och kan hantera stora objekt, medan Gson är enklare att använda för mindre projekt. `java.json` standardiserar hur Java hanterar JSON utan tredjepartsbibliotek, men tredjepartslösningar erbjuder ibland mer funktioner och flexibilitet.

## See Also:
- [JSON.org](https://www.json.org/json-en.html) – JSON specifikationen.
- [Jackson Project](https://github.com/FasterXML/jackson) – Jacksons GitHub sida.
- [Google Gson](https://github.com/google/gson) – Gsons GitHub sida.
- [Java API for JSON Processing](https://javaee.github.io/jsonp/) – Information om Java API för JSON Processing (`javax.json`).
