---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:04.571921-07:00
description: "Hur man g\xF6r: L\xE5t oss kavla upp \xE4rmarna och b\xF6rja koda med\
  \ JSON i Java. F\xF6r det f\xF6rsta, du beh\xF6ver ett JSON-behandlande bibliotek\
  \ som `Jackson` eller\u2026"
lastmod: '2024-03-13T22:44:37.808551-06:00'
model: gpt-4-0125-preview
summary: "L\xE5t oss kavla upp \xE4rmarna och b\xF6rja koda med JSON i Java."
title: Arbeta med JSON
weight: 38
---

## Hur man gör:
Låt oss kavla upp ärmarna och börja koda med JSON i Java.

För det första, du behöver ett JSON-behandlande bibliotek som `Jackson` eller `Google Gson`. Här kommer vi att använda `Jackson`, så lägg till detta beroende i din `pom.xml`:

```xml
<dependency>
    <groupId>com.fasterxml.jackson.core</groupId>
    <artifactId>jackson-databind</artifactId>
    <version>2.13.1</version>
</dependency>
```

Nu, låt oss serialisera (skriva) ett enkelt Java-objekt till JSON:

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

Utmatningen bör vara:

```json
{"name":"Alex","age":30}
```

Nu, för att avserialisera (läsa) JSON tillbaka till ett Java-objekt:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        String json = "{\"name\":\"Alex\",\"age\":30}";
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = mapper.readValue(json, Person.class);
            System.out.println(person.name + " är " + person.age + " år gammal.");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Utmatningen kommer att vara:

```
Alex är 30 år gammal.
```

## Fördjupning
JSONs enkelhet och effektivitet har gjort det till standarden för datautbyte på webben, vilket har omkullkastat XML från dess tron. Introducerat i början av 2000-talet, härstammar JSON från JavaScript men stöds nu i de flesta språk.

Alternativ till JSON inkluderar XML, som är mer verbose, och binära format som Protocol Buffers eller MessagePack, som är mindre läsbara för människor men mer effektiva i storlek och hastighet. Varje har sina användningsområden; valet beror på dina specifika databehov och sammanhang.

I Java, bortom `Jackson` och `Gson`, har vi `JsonB` och `org.json` som andra bibliotek för att hantera JSON. Jackson erbjuder strömbaserad bearbetning och är känt för hastighet, medan Gson är hyllat för sin enkelhet i användning. JsonB är en del av Jakarta EE, som erbjuder en mer standardiserad metod.

När du implementerar JSON, kom ihåg att hantera dina undantag på rätt sätt - din kod bör vara robust mot dåliga inmatningar. Överväg även säkerhetsaspekterna av automatisk databindning – validera alltid dina indata!

## Se även
- [Jackson-projektet](https://github.com/FasterXML/jackson)
- [Gson-projektet](https://github.com/google/gson)
- [JSON-specifikationen](https://www.json.org/json-en.html)
- [JsonB-specifikationen](https://jakarta.ee/specifications/jsonb/)
