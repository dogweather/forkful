---
title:                "Working with JSON"
html_title:           "Arduino recipe: Working with JSON"
simple_title:         "Working with JSON"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON (JavaScript Object Notation) is a text format to transfer and store data. Programmers dig it 'cause it's human-readable, lightweight, and the bread and butter for web APIs and configs. 

## How to:
```java
import com.fasterxml.jackson.databind.ObjectMapper; // Jackson library
import java.util.Map;

public class JsonExample {
    public static void main(String[] args) throws Exception {
        // Convert JSON string to Map
        String jsonString = "{\"name\":\"Coder Duck\",\"age\":5}";
        ObjectMapper mapper = new ObjectMapper();
        Map<String, Object> data = mapper.readValue(jsonString, Map.class);
        System.out.println(data); // Prints: {name=Coder Duck, age=5}
        
        // Convert Map to JSON string
        String jsonOutput = mapper.writeValueAsString(data);
        System.out.println(jsonOutput); // Prints: {"name":"Coder Duck","age":5}
    }
}
```

## Deep Dive
JSON was born in the early 2000s, answering the need for an easy data interchange format. XML was a contender but JSON's lack of verbosity won the crowd. Jackson and Gson are hot Java libs for JSON. While Jackson's the heavy lifter, Gson is the nimble counterpart. Under the hood, they convert between Strings and Java objects with reflection and type tokens.

## See Also
- [Jackson Core](https://github.com/FasterXML/jackson-core): Get the latest on the Jackson library.
- [Gson GitHub repo](https://github.com/google/gson): Dive into Gson.
- [JSON.org](https://www.json.org/json-en.html): JSON's official crib.
- [Baeldung on JSON in Java](https://www.baeldung.com/java-json): More examples and tutorials.
