---
title:                "Arbeide med yaml"
html_title:           "C#: Arbeide med yaml"
simple_title:         "Arbeide med yaml"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

YAML er en populær måte å lagre og strukturere data på, spesielt innenfor programmering. Det er enkelt å lese og skrive, og er et praktisk valg for å organisere kompleks informasjon.

## Slik gjør du det

Vi kan bruke C# til å arbeide med YAML-filer ved å bruke et tredjeparts bibliotek, for eksempel YamlDotNet. Først må vi installere dette biblioteket ved å legge til pakken i prosjektet vårt. Deretter kan vi begynne å lese og skrive YAML-filer ved hjelp av følgende kode:

```csharp
var input = new StreamReader("example.yml"); // Åpner YAML-filen for lesing
var deserializer = new Deserializer(); // Oppretter en deserializer-instans
var yamlObject = deserializer.Deserialize(input); // Deserialiserer YAML-innholdet til et YAML-objekt
var output = new Dictionary<string, object>(yamlObject); // Konverterer YAML-objektet til et Dictionary-objekt
```

Vi kan også skrive YAML-filer ved hjelp av følgende kode:

```csharp
var dictionary = new Dictionary<string, object>(); // Oppretter et tomt Dictionary-objekt
dictionary.Add("key1", "value1"); // Legger til nøkler og verdier
dictionary.Add("key2", "value2");
var serializer = new Serializer(); // Oppretter en serializer-instans
var yaml = serializer.Serialize(dictionary); // Serialiserer Dictionary-objektet til YAML-format
File.WriteAllText("example.yml", yaml); // Skriver YAML-data til fil
```

Etter å ha lest og skrevet YAML-filer, kan vi også endre eksisterende YAML-data ved å bruke samme metoder som nevnt ovenfor. Dette gjør det veldig fleksibelt og enkelt å arbeide med YAML-formater i C#.

## Dypdykk

YAML står for "YAML Ain't Markup Language" og er et lettvektig og leselig datautvekslingsspråk. Den følger et menneskelesbart format som gjør det enkelt for både mennesker og datamaskiner å tolke. YAML brukes ofte til å konfigurere programmer og verktøy, men kan også brukes til å strukturere data som skal leses av programmer.

For å lære mer om YAML og hvordan du kan bruke det i dine prosjekter, kan du sjekke ut følgende ressurser:

- [YAML-specifikasjonen](https://yaml.org/spec/1.2/spec.html) - Den offisielle YAML-spesifikasjonen som definerer syntaksen og strukturen til YAML.
- [YamlDotNet dokumentasjon](https://github.com/aaubry/YamlDotNet/wiki) - Offisiell dokumentasjon for YamlDotNet-biblioteket.
- [YAML Tutorial](https://rollout.io/blog/yaml-tutorial-everything-you-need-get-started/) - En enkel og nyttig guide for å forstå og bruke YAML.

## Se også

- [YAML vs. JSON: Hvilken er best for ditt prosjekt?](https://www.freecodecamp.org/news/yaml-vs-json-which-is-the-better-for-your-project/) - En sammenligning av YAML og JSON og når du bør bruke dem.
- [Hvordan arbeide med JSON-data i C#](https://dev.to/hassanhabib/handling-json-objects-in-c-a-beginners-tutorial-26p7) - En artikkel om å arbeide med JSON-data i C#.