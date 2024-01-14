---
title:                "C#: Arbeid med json"
simple_title:         "Arbeid med json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

#Hvorfor
Hvis du jobber med C#, vil du sannsynligvis komme over JSON på et eller annet punkt. JSON er et tekstformat for å lagre og utveksle data, og det er mye brukt i dagens webutvikling. Det kan være svært nyttig for å lagre store mengder data og overføre det mellom klienter og servere.

#Hvordan
For å jobbe med JSON i C#, må du først importere "System.Json" -biblioteket. Deretter kan du bruke metoder som "DeserializeObject" og "SerializeObject" for å konvertere JSON-data til objekter og omvendt.

```C#
using System.Json;

//Opprette et JSON-objekt
JsonObject person = new JsonObject();
person.Add("navn", "Ingrid");
person.Add("alder", 30);

//Konvertere JSON til en streng
string jsonTekst = person.ToString();

//Konvertere en streng til JSON
JsonObject nyPerson = (JsonObject)JsonValue.Parse(jsonTekst);
```

#Dypdykk
Det finnes mange forskjellige metoder for å jobbe med JSON i C#, avhengig av dine behov og preferanser. Noen foretrekker å bruke tredjepartsbiblioteker som Newtonsoft.Json, mens andre foretrekker å skrive egne klasser for å håndtere konvertering. Uansett hva du velger, er det viktig å forstå hvordan JSON fungerer og hvordan du kan tilpasse det til dine behov.

#Se også
- [Microsoft Docs: Working with JSON in C#](https://docs.microsoft.com/en-us/dotnet/standard/serialization/system-text-json-how-to)
- [Newtonsoft.Json Documentation](https://www.newtonsoft.com/json/help/html/Introduction.htm)
- [JSON.org](https://www.json.org/)