---
title:                "Arbeid med json"
html_title:           "C#: Arbeid med json"
simple_title:         "Arbeid med json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?

Å jobbe med JSON er å håndtere data i et bestemt format som brukes til å utveksle og lagre informasjon. Dette er viktig for programmerere fordi det tillater effektiv lagring og utveksling av data mellom ulike programmer og plattformer.

Slik gjør du det:

Å jobbe med JSON i C# er ganske enkelt. Følg disse trinnene:

1. Importer Newtonsoft.Json biblioteket.
2. Definer et JSON-objekt.
3. Foreta konverteringer mellom JSON og .NET-objekter ved hjelp av JsonConvert-klassen.

Et eksempel på hvordan dette kan gjøres ser slik ut:

```C#
// Importer biblioteket
using Newtonsoft.Json;

// Definer JSON-objekt
string json = "{'Navn': 'Lars', 'Alder': 28}";

// Konverter fra JSON til .NET-objekt
Person p = JsonConvert.DeserializeObject<Person>(json);

// Konverter fra .NET-objekt til JSON
string nyttJson = JsonConvert.SerializeObject(p);

```

Du kan også bruke LINQ til å utforske og manipulere JSON-data på en enkel måte.

Dypdykk:

JSON ble opprinnelig utviklet av Douglas Crockford i 2001 og har siden blitt et populært format for utveksling og lagring av data. Selv om XML også brukes til dette formålet, er JSON mer lettlest, kompakt og effektivt.

Som et alternativ til å jobbe med JSON i C#, kan du også bruke JavaScriptSerializer-klassen som er innebygd i .NET-rammeverket. Denne klassen tilbyr lignende funksjonalitet, men har ikke like mange muligheter som JsonConvert-klassen fra Newtonsoft.Json.

Se også:

- [Newtonsoft.Json Documentation](https://www.newtonsoft.com/json/help/html/SerializingJSON.htm)