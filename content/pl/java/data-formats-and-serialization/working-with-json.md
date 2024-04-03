---
date: 2024-01-19
description: "How to: U\u017Cyjemy biblioteki `Jackson` do obs\u0142ugi JSON w Java.\
  \ Najpierw dodaj zale\u017Cno\u015B\u0107 do `pom.xml`."
lastmod: '2024-03-13T22:44:35.300242-06:00'
model: unknown
summary: "U\u017Cyjemy biblioteki `Jackson` do obs\u0142ugi JSON w Java."
title: Praca z JSON
weight: 38
---

## How to:
Użyjemy biblioteki `Jackson` do obsługi JSON w Java. Najpierw dodaj zależność do `pom.xml`:

```xml
<dependency>
    <groupId>com.fasterxml.jackson.core</groupId>
    <artifactId>jackson-databind</artifactId>
    <version>2.13.1</version>
</dependency>
```

Teraz parsuj JSON i zapisz do obiektu:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        String jsonInput = "{\"name\":\"Jan\",\"age\":30}";
        ObjectMapper mapper = new ObjectMapper();

        try {
            User user = mapper.readValue(jsonInput, User.class);
            System.out.println("Imię: " + user.getName());
            System.out.println("Wiek: " + user.getAge());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

class User {
    private String name;
    private int age;
    // Gettery i settery
}
```
Wynik:
```
Imię: Jan
Wiek: 30
```

Generuj JSON z obiektu:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        User user = new User("Ania", 25);
        ObjectMapper mapper = new ObjectMapper();

        try {
            String jsonOutput = mapper.writeValueAsString(user);
            System.out.println(jsonOutput);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

class User {
    private String name;
    private int age;
    // Konstruktor, gettery i settery
}
```

Wynik:
```
{"name":"Ania","age":25}
```

## Deep Dive
JSON pojawił się w 2001 roku jako alternatywa dla XML. Jest prostszy i szybszy w przetwarzaniu. Oprócz `Jacksona`, inne popularne biblioteki do obsługi JSON w Javie to `Gson` i `JSONP`. Jackson jest szybki i przyjmuje różne podejścia do parsowania, wliczając strumieniowe API i model na drzewo.

## See Also
- [Jackson Project](https://github.com/FasterXML/jackson)
- [Tutorial Jackson](https://www.baeldung.com/jackson)
- [Specyfikacja JSON](https://www.json.org/json-pl.html)
- [Porównanie bibliotek JSON w Javie](https://www.baeldung.com/java-json)
