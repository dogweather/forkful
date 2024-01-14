---
title:                "C#: Arbeide med yaml"
simple_title:         "Arbeide med yaml"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

YAML er et populært formatteringsspråk som brukes til å strukturere data i en leselig form. Det kan være nyttig for utviklere å lære YAML for å organisere og lagre konfigurasjonsfiler og data på en mer effektiv måte.

## Hvordan

For å begynne å jobbe med YAML i C# må du først installere en pakke kalt "YamlDotNet" via NuGet Package Manager. Deretter må du importere namespace for YamlDotNet ved å bruke følgende kode:

```C#
using YamlDotNet.Serialization;
```

For å lese en YAML-fil i C# kan du bruke følgende kode:

```C#
var deserializer = new DeserializerBuilder().Build();
var yamlObject = deserializer.Deserialize<object>(File.OpenText("file.yaml"));
```

For å skrive data til en YAML-fil kan du bruke følgende kode:

```C#
var serializer = new SerializerBuilder().Build();
using (var writer = new StringWriter())
{
    serializer.Serialize(writer, dataObject);
    var yamlString = writer.ToString();
}
```

Et eksempel på YAML-fil og dens ekvivalente C# objekt kan sees nedenfor:

```C#
# YAML-fil
name: John
age: 25
programming_languages:
    - C#
    - Java
    - Python
```

```C#
// C# objekt
var person = new {
    name = "John",
    age = 25,
    programming_languages = new string[] { "C#", "Java", "Python" }
};
```

Når du har skrevet eller lest data fra en YAML-fil kan du enkelt bruke de tilsvarende C# objektene i koden din.

## Dypdykk

YAML støtter også mer avanserte funksjoner som å definere egendefinerte datatyper og referanser til andre deler av YAML-filen. Den har også støtte for kommentarer, inkludering av filer og mulighet for å parsere data fra JSON.

Det er viktig å merke seg at YAML er et strengt nivåbasert språk, noe som betyr at riktig formatering og innrykk er viktig for å sikre at dataene blir tolket riktig. Det finnes også ulike versjoner av YAML, så det er viktig å sjekke hvilken versjon som er støttet av biblioteket du bruker.

## Se også

- [YamlDotNet på GitHub](https://github.com/aaubry/YamlDotNet)
- [YAML-spesifikasjonen](https://yaml.org/spec/)
- [Offisiell YAML-nettside](https://yaml.org/)