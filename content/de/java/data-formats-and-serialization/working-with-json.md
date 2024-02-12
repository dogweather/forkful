---
title:                "Arbeiten mit JSON"
aliases: - /de/java/working-with-json.md
date:                  2024-02-03T19:23:35.079251-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Mit JSON (JavaScript Object Notation) zu arbeiten bedeutet, dieses leichte Datenaustauschformat in Ihren Java-Anwendungen zu handhaben. Programmierer entscheiden sich für JSON, um strukturierte Daten über ein Netzwerk zu serialisieren und zu übertragen sowie Daten leicht zu konfigurieren und zu speichern, da es menschenlesbar und sprachunabhängig ist.

## Wie:
Lasst uns die Ärmel hochkrempeln und mit der Programmierung mit JSON in Java beginnen.

Zuerst benötigen Sie eine JSON-Verarbeitungsbibliothek wie `Jackson` oder `Google Gson`. Hier verwenden wir `Jackson`, also fügen Sie diese Abhängigkeit zu Ihrer `pom.xml` hinzu:

```xml
<dependency>
    <groupId>com.fasterxml.jackson.core</groupId>
    <artifactId>jackson-databind</artifactId>
    <version>2.13.1</version>
</dependency>
```

Jetzt serialisieren wir (schreiben) ein einfaches Java-Objekt nach JSON:

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

Die Ausgabe sollte sein:

```json
{"name":"Alex","age":30}
```

Nun, um JSON wieder in ein Java-Objekt zu deserialisieren (lesen):

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        String json = "{\"name\":\"Alex\",\"age\":30}";
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = mapper.readValue(json, Person.class);
            System.out.println(person.name + " ist " + person.age + " Jahre alt.");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Die Ausgabe wird sein:

```
Alex ist 30 Jahre alt.
```

## Tiefergehend
Die Einfachheit und Effektivität von JSON haben es zum de facto Standard für den Datenaustausch im Web gemacht und XML von seinem Thron gestoßen. Eingeführt in den frühen 2000ern, wurde JSON von JavaScript abgeleitet, wird aber nun von den meisten Sprachen unterstützt.

Alternativen zu JSON sind XML, das mehrschreibend ist, und binäre Formate wie Protocol Buffers oder MessagePack, die weniger menschenlesbar, aber in Größe und Geschwindigkeit effizienter sind. Jedes hat seine Anwendungsfälle; die Wahl hängt von Ihren spezifischen Datenbedürfnissen und dem Kontext ab.

In Java haben wir über `Jackson` und `Gson` hinaus auch `JsonB` und `org.json` als andere Bibliotheken, um mit JSON umzugehen. Jackson bietet streambasierte Verarbeitung und ist bekannt für seine Geschwindigkeit, während Gson für seine Benutzerfreundlichkeit gefeiert wird. JsonB ist Teil von Jakarta EE und bietet einen standardisierteren Ansatz.

Wenn Sie JSON implementieren, denken Sie daran, Ihre Ausnahmen ordnungsgemäß zu behandeln – Ihr Code sollte robust gegen schlechte Eingaben sein. Betrachten Sie auch die Sicherheitsimplikationen des automatischen Datenbindings – validieren Sie immer Ihre Eingaben!

## Siehe auch
- [Jackson-Projekt](https://github.com/FasterXML/jackson)
- [Gson-Projekt](https://github.com/google/gson)
- [JSON-Spezifikation](https://www.json.org/json-en.html)
- [JsonB-Spezifikation](https://jakarta.ee/specifications/jsonb/)
