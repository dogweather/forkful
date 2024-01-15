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

## Hvorfor
Har du noen gang ønsket å kommunisere data på en enkel og effektiv måte mellom klient og server? JSON (JavaScript Object Notation) er et populært format for å gjøre nettopp det. Det er enkelt å lese og skrive, og blir stadig mer brukt i moderne webutvikling.

## Hvordan
For å arbeide med JSON i Java, trenger du et bibiliotek kalt "json.jar". Dette kan lastes ned fra nettet og må deretter legges til i prosjektet ditt som en ekstern JAR-fil. Etter dette kan du bruke følgende kode for å lese inn JSON-data fra en fil og skrive den ut på konsollen:

```Java
try {
    // Leser inn JSON-data fra fil
    FileReader file = new FileReader("data.json");
    // Bruker JSON-biblioteket til å parse dataene
    JSONParser parser = new JSONParser();
    // Lagrer dataene i et objekt
    Object obj = parser.parse(file);
    // Konverterer objektet til en JSON-objekt
    JSONObject jsonObject = (JSONObject) obj;
    // Skriver ut dataene på konsollen
    System.out.println(jsonObject);
} catch (Exception e) {
    e.printStackTrace();
}
```

Dette vil gi følgende utskrift:

``` 
{"navn":"Per", "alder": 30, "hobbyer":["fotball", "musikk"]}
```

For å skrive til en JSON-fil, kan du bruke følgende kode:

```Java
// Oppretter et JSON-objekt
JSONObject jsonObject = new JSONObject();
// Legger til data
jsonObject.put("navn", "Lisa");
jsonObject.put("alder", 25);
jsonObject.put("hobbyer", "matlaging");
// Skriver ut til konsollen
System.out.println(jsonObject);
// Skriver til fil
try (FileWriter file = new FileWriter("ny_data.json")) {
    file.write(jsonObject.toJSONString());
} catch (Exception e) {
    e.printStackTrace();
}
```

Dette vil skrive følgende JSON-innhold til filen "ny_data.json":

``` 
{"navn":"Lisa", "alder": 25, "hobbyer":"matlaging"}
```

## Dypdykk
Nå som du vet hvordan du kan lese og skrive JSON-data i Java, kan du også utforske mer avanserte funksjoner som å endre eller legge til data, eller å bruke forskjellige typer datastrukturer som arrays og nestede objekter. Du kan også lese mer om validering av JSON-data og håndtering av eventuelle feil og unntak.

## Se også
- [Java-bibliotek for JSON](https://code.google.com/archive/p/json-simple/downloads)
- [Offisiell nettside for JSON](https://www.json.org/json-en.html)
- [Tutorial: Working with JSON in Java](https://www.tutorialspoint.com/json/json_java_example.htm)