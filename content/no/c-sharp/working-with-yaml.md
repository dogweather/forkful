---
title:                "Arbeid med yaml"
html_title:           "C#: Arbeid med yaml"
simple_title:         "Arbeid med yaml"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
YAML står for "Yet Another Markup Language" og er et format for strukturert data som brukes av mange programmerere. Det er en komprimert og lesbar måte å representere data på, og er spesielt nyttig for konfigurasjonsfiler og datautveksling.

# Hvordan?
YAML er enkelt å bruke fordi det har et intuitivt og minimalistisk syntaks. Her er et eksempel på en enkel YAML-fil som lagrer informasjon om en person:

```C#
navn: Martin
alder: 24
jobb: utvikler
```

Som du kan se, bruker YAML nøkkel-verdi-par for å representere data. Dette gjør det lett å lese og forstå, selv for ikke-programmerere.

# Dykk Dypt
YAML ble først introdusert i 2001 og har siden blitt populært på grunn av sin enkelhet. I tillegg til å være et praktisk format for konfigurasjonsfiler og dataoverføring, er det også mange alternativer til YAML, som for eksempel JSON og XML.

For å jobbe med YAML i C#, kan du bruke et tredjeparts bibliotek som "YamlDotNet" som lar deg serialisere og deserialisere YAML-filer til C# objekter. Du kan også bruke innebygde metoder, som "YamlSerializer", for å jobbe med YAML-data.

# Se også
Hvis du vil lære mer om YAML og hvordan du kan bruke det i C#, bør du sjekke ut følgende ressurser:

- YamlDotNet biblioteket: https://github.com/aaubry/YamlDotNet
- Dokumentasjon for YamlSerializer klasse: https://docs.microsoft.com/en-us/dotnet/api/system.yaml.yamlserializer