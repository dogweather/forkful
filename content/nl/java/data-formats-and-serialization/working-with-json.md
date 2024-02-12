---
title:                "Werken met JSON"
aliases: - /nl/java/working-with-json.md
date:                  2024-01-28T22:10:47.483863-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/java/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Werken met JSON (JavaScript Object Notation) betekent het hanteren van dit lichtgewicht gegevensuitwisselingsformaat binnen uw Java-applicaties. Programmeurs kiezen voor JSON om gestructureerde gegevens te serialiseren en over een netwerk te verzenden en om gegevens gemakkelijk te configureren en op te slaan, omdat het leesbaar en taalonafhankelijk is.

## Hoe te:
Laten we onze mouwen oprollen en beginnen met coderen met JSON in Java.

Allereerst hebt u een JSON-verwerkingsbibliotheek nodig zoals `Jackson` of `Google Gson`. Hier zullen we `Jackson` gebruiken, dus voeg deze afhankelijkheid toe aan uw `pom.xml`:

```xml
<dependency>
    <groupId>com.fasterxml.jackson.core</groupId>
    <artifactId>jackson-databind</artifactId>
    <version>2.13.1</version>
</dependency>
```

Nu gaan we een eenvoudig Java-object serialiseren (schrijven) naar JSON:

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

De output zou moeten zijn:

```json
{"name":"Alex","age":30}
```

Nu, om JSON te deserialiseren (lezen) terug naar een Java-object:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        String json = "{\"name\":\"Alex\",\"age\":30}";
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = mapper.readValue(json, Person.class);
            System.out.println(person.name + " is " + person.age + " jaar oud.");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

De output zal zijn:

```
Alex is 30 jaar oud.
```

## Diepere Duik
De eenvoud en effectiviteit van JSON hebben het tot de de facto standaard gemaakt voor gegevensuitwisseling op het web, waardoor XML van zijn troon is gestoten. Geïntroduceerd in de vroege jaren 2000, werd JSON afgeleid van JavaScript, maar wordt nu ondersteund in de meeste talen.

Alternatieven voor JSON zijn onder andere XML, dat uitgebreider is, en binaire formaten zoals Protocol Buffers of MessagePack, die minder leesbaar zijn voor mensen maar efficiënter in grootte en snelheid. Elk heeft zijn gebruiksscenario's; de keuze hangt af van uw specifieke gegevensbehoeften en context.

In Java hebben we, naast `Jackson` en `Gson`, ook `JsonB` en `org.json` als andere bibliotheken om JSON te hanteren. Jackson biedt streamgebaseerde verwerking en staat bekend om zijn snelheid, terwijl Gson wordt gevierd vanwege het gebruiksgemak. JsonB maakt deel uit van Jakarta EE en biedt een meer gestandaardiseerde aanpak.

Bij het implementeren van JSON, vergeet niet om uitzonderingen correct te behandelen - uw code moet robuust zijn tegen slechte invoer. Overweeg ook de beveiligingsimplicaties van automatische gegevensbinding – valideer altijd uw invoer!

## Zie Ook
- [Jackson Project](https://github.com/FasterXML/jackson)
- [Gson Project](https://github.com/google/gson)
- [JSON Specificatie](https://www.json.org/json-en.html)
- [JsonB Specificatie](https://jakarta.ee/specifications/jsonb/)
