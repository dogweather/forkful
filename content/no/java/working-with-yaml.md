---
title:                "Å jobbe med yaml"
html_title:           "Java: Å jobbe med yaml"
simple_title:         "Å jobbe med yaml"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/working-with-yaml.md"
---

{{< edit_this_page >}}

YAML: Hva er det og hvorfor bruker programmerere det?

Hva & hvorfor?

YAML står for "YAML Ain't Markup Language" og er et strukturert dataformat som brukes til å lagre og overføre data mellom programmer. YAML er enklere å lese og forstå for mennesker sammenlignet med andre dataformater som XML og JSON, noe som gjør det mer brukervennlig for programmerere. Det brukes ofte til konfigurasjonsfiler og lagring av data i webapplikasjoner.

Hvordan:

YAML bruker en syntaks som ligner på vanlig talespråk og er enkelt å lese og forstå. Her er et eksempel på hvordan man kan lage og lese en YAML-fil i Java:

```Java
// Opprette en YAML-fil
Yaml yaml = new Yaml();
Map<String, Object> data = new HashMap<>();
data.put("navn", "Lisa");
data.put("alder", 26);
data.put("interesser", new String[]{"Programmering", "Fotografering", "Reise"});

yaml.dump(data, new FileWriter("minfil.yaml"));

// Lese fra en YAML-fil
Map<String, Object> result = (Map<String, Object>) yaml.load(new FileReader("minfil.yaml"));

String navn = (String) result.get("navn");
int alder = (int) result.get("alder");
String[] interesser = (String[]) result.get("interesser");

System.out.println("Navn: " + navn);
System.out.println("Alder: " + alder);
System.out.println("Interesser: " + Arrays.toString(interesser));
```

Output:
```
Navn: Lisa
Alder: 26
Interesser: Programmering, Fotografering, Reise
```

Deep Dive:

YAML ble først introdusert i 2001 og har siden blitt et populært valg for å strukturere data i ulike programmeringsspråk og plattformer. Det er et åpent format som er enkelt å implementere, pakke og overføre mellom forskjellige systemer. YAML kan også brukes til å definere objekter og deres egenskaper, noe som gjør det nyttig for konfigurasjonsfiler i komplekse applikasjoner.

Alternativer til YAML inkluderer XML, som er mye mer komplekst og vanskeligere å lese, og JSON, som ikke støtter kommentarer og kræsjer om ikke alle felter er deklarert riktig. Mange programmerere foretrekker YAML på grunn av dets enkelhet og fleksibilitet.

See Also:

For mer informasjon om YAML-formatet og bruken av det i Java, kan du sjekke ut følgende kilder:

- [YAML offisiell nettside](https://yaml.org/)
- [YAML for nybegynnere](https://medium.com/@dgrdnr/what-the-heck-is-yaml-931b0302a654)
- [YAML i Java med SnakeYAML](https://www.baeldung.com/java-snake-yaml)