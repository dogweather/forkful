---
date: 2024-01-25 03:57:30.429534-07:00
description: "Working with JSON (JavaScript Object Notation) means handling this lightweight\
  \ data-interchange format inside your Java applications. Programmers go for\u2026"
lastmod: '2024-02-25T18:49:56.433879-07:00'
model: gpt-4-1106-preview
summary: "Working with JSON (JavaScript Object Notation) means handling this lightweight\
  \ data-interchange format inside your Java applications. Programmers go for\u2026"
title: Working with JSON
---

{{< edit_this_page >}}

## What & Why?
Working with JSON (JavaScript Object Notation) means handling this lightweight data-interchange format inside your Java applications. Programmers go for JSON to serialize and transmit structured data over a network and easily configure and store data because it's human-readable and language-independent.

## How to:
Let's roll up our sleeves and get to coding with JSON in Java.

First thing, you'll need a JSON processing library like `Jackson` or `Google Gson`. Here we'll use `Jackson`, so add this dependency to your `pom.xml`:

```xml
<dependency>
    <groupId>com.fasterxml.jackson.core</groupId>
    <artifactId>jackson-databind</artifactId>
    <version>2.13.1</version>
</dependency>
```

Now, let’s serialize (write) a simple Java object to JSON:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = new Person("Alex", 30);
            String json = mapper.writeValueAsString(person);
            System.out.println(json);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

class Person {
    public String name;
    public int age;

    public Person(String name, int age) {
        this.name = name;
        this.age = age;
    }
}
```

Output should be:

```json
{"name":"Alex","age":30}
```

Now, to deserialize (read) JSON back into a Java object:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        String json = "{\"name\":\"Alex\",\"age\":30}";
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = mapper.readValue(json, Person.class);
            System.out.println(person.name + " is " + person.age + " years old.");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Output will be:

```
Alex is 30 years old.
```

## Deep Dive
JSON's simplicity and effectiveness have made it the de facto standard for data exchange on the web, toppling XML from its throne. Introduced in the early 2000s, JSON was derived from JavaScript but is now supported across most languages.

Alternatives to JSON include XML, which is more verbose, and binary formats like Protocol Buffers or MessagePack, which are less human-readable but more efficient in size and speed. Each has their use cases; the choice depends on your specific data needs and context.

In Java, beyond `Jackson` and `Gson`, we've got `JsonB` and `org.json` as other libraries to handle JSON. Jackson offers stream-based processing and is known for speed, while Gson is celebrated for its ease of use. JsonB is part of Jakarta EE, offering a more standardized approach.

When implementing JSON, remember to handle your exceptions properly - your code should be robust against bad inputs. Also, consider the security implications of automatic data binding – always validate your inputs!

## See Also
- [Jackson Project](https://github.com/FasterXML/jackson)
- [Gson Project](https://github.com/google/gson)
- [JSON Specification](https://www.json.org/json-en.html)
- [JsonB Specification](https://jakarta.ee/specifications/jsonb/)
