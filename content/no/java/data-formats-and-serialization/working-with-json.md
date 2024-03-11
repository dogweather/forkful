---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:29.236228-07:00
description: "\xC5 jobbe med JSON (JavaScript Object Notation) betyr \xE5 h\xE5ndtere\
  \ dette lette datautvekslingsformatet inne i Java-applikasjonene dine. Programmerere\
  \ velger\u2026"
lastmod: '2024-03-11T00:14:14.231999-06:00'
model: gpt-4-0125-preview
summary: "\xC5 jobbe med JSON (JavaScript Object Notation) betyr \xE5 h\xE5ndtere\
  \ dette lette datautvekslingsformatet inne i Java-applikasjonene dine. Programmerere\
  \ velger\u2026"
title: Arbeider med JSON
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å jobbe med JSON (JavaScript Object Notation) betyr å håndtere dette lette datautvekslingsformatet inne i Java-applikasjonene dine. Programmerere velger JSON for å serialisere og overføre strukturerte data over et nettverk og enkelt konfigurere og lagre data fordi det er menneskelesbart og språkuavhengig.

## Hvordan:
La oss brette opp ermene og begynne å kode med JSON i Java.

Først og fremst trenger du et JSON-prosesseringsbibliotek som `Jackson` eller `Google Gson`. Her vil vi bruke `Jackson`, så legg til denne avhengigheten i `pom.xml`:

```xml
<dependency>
    <groupId>com.fasterxml.jackson.core</groupId>
    <artifactId>jackson-databind</artifactId>
    <version>2.13.1</version>
</dependency>
```

Nå, la oss serialisere (skrive) et enkelt Java-objekt til JSON:

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

Output bør være:

```json
{"name":"Alex","age":30}
```

Nå, for å deserialisere (lese) JSON tilbake til et Java-objekt:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        String json = "{\"name\":\"Alex\",\"age\":30}";
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = mapper.readValue(json, Person.class);
            System.out.println(person.name + " er " + person.age + " år gammel.");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Output vil være:

```
Alex er 30 år gammel.
```

## Dypdykk
JSONs enkelhet og effektivitet har gjort det til standarden for datautveksling på nett, og har avsatt XML fra tronen. Introdusert på begynnelsen av 2000-tallet, ble JSON avledet fra JavaScript, men støttes nå på tvers av de fleste språk.

Alternativer til JSON inkluderer XML, som er mer verbost, og binære formater som Protocol Buffers eller MessagePack, som er mindre menneskelesbare, men mer effektive i størrelse og hastighet. Hver har sine bruksområder; valget avhenger av dine spesifikke databehov og kontekst.

I Java, utover `Jackson` og `Gson`, har vi `JsonB` og `org.json` som andre biblioteker for å håndtere JSON. Jackson tilbyr strømbasert behandling og er kjent for hastighet, mens Gson er feiret for sin brukervennlighet. JsonB er en del av Jakarta EE, som tilbyr en mer standardisert tilnærming.

Når du implementerer JSON, husk å håndtere unntakene dine riktig - koden din bør være robust mot dårlige inndata. Vurder også sikkerhetsimplikasjonene av automatisk databinding - valider alltid inndataene dine!

## Se Også
- [Jackson-prosjektet](https://github.com/FasterXML/jackson)
- [Gson-prosjektet](https://github.com/google/gson)
- [JSON-spesifikasjonen](https://www.json.org/json-en.html)
- [JsonB-spesifikasjonen](https://jakarta.ee/specifications/jsonb/)
