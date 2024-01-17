---
title:                "Å jobbe med json"
html_title:           "Java: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/working-with-json.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?
Å jobbe med JSON er å behandle data ved hjelp av et format kalt JavaScript Object Notation (JSON). Dette blir ofte brukt for å utveksle data mellom forskjellige systemer og programmeringsspråk. Programmerere bruker JSON fordi det er enklere å lese og skrive enn andre dataformater, som XML.

Hvordan:
For å jobbe med JSON i Java, må du bruke et bibliotek som heter Jackson. Først må du legge til denne avhengigheten i pom.xml-filen din: 

```Java
<!-- Jackson -->
<dependency>
  <groupId>com.fasterxml.jackson.core</groupId>
  <artifactId>jackson-databind</artifactId>
  <version>2.11.4</version>
</dependency>
```

Deretter kan du bruke kode som dette for å mappe JSON-data til Java-objekter:

```Java
ObjectMapper mapper = new ObjectMapper();
MyObject obj = mapper.readValue(jsonString, MyObject.class);
```

Hvis du vil lage JSON fra et Java-objekt, kan du gjøre dette:

```Java
ObjectMapper mapper = new ObjectMapper();
MyObject obj = new MyObject();
String jsonString = mapper.writeValueAsString(obj);
```

Dypdykk:
JSON ble opprinnelig utviklet i 1999 som en enklere måte å strukturere data på sammenlignet med XML. Det finnes også andre alternativer for å behandle data, som for eksempel CSV og YAML. JSON har blitt et populært valg fordi det er enklere å lese og skrive, og det er godt støttet av de fleste programmeringsspråk og systemer.

Se også:
- Jackson: https://github.com/FasterXML/jackson
- Baeldung sine Jackson tutorials: https://www.baeldung.com/jackson