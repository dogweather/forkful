---
title:                "Praca z yaml"
html_title:           "C#: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pracowanie z YAML jest popularną praktyką wśród programistów. Jest to język znaczników, który jest wykorzystywany do wygodnego przechowywania i udostępniania danych. Programiści wykorzystują YAML, aby zapewnić prosty i czytelny sposób na przechowywanie i przekazywanie informacji w swoich projektach.

## Jak to zrobić:

Kodowanie przy użyciu YAML w języku C# jest proste i wygodne. Przykładowy kod i efekt znajdują się poniżej:

```C#
using System;
using System.IO;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

namespace YAMLExample
{
    class Program
    {
        static void Main(string[] args)
        {
            // Przykładowy obiekt do serializacji
            var person = new
            {
                Name = "John",
                Age = 30,
                Occupation = "Software Developer"
            };

            // Serializacja obiektu do formatu YAML
            var serializer = new SerializerBuilder()
                .WithNamingConvention(CamelCaseNamingConvention.Instance)
                .Build();
            var yaml = serializer.Serialize(person);

            // Wyświetlenie wyniku
            Console.WriteLine(yaml);
        }
    }
}
```

Wynik:

```md
name: John
age: 30
occupation: Software Developer
```

## Głębsze zanurzenie:

YAML został stworzony w celu zastąpienia formatów XML i JSON jako ustandaryzowanej metody przekazywania informacji. Jest on bardzo czytelny dla człowieka ze względu na swoją składnię, która przypomina język naturalny. Alternatywnymi formatami są JSON i XML, jednak YAML oferuje prostszą i czytelniejszą strukturę. Aby wykorzystać YAML w swoim projekcie, programiści muszą zainstalować odpowiednią bibliotekę, taką jak ta wykorzystana w przykładzie powyżej.

## Zobacz również:

- [Dokumentacja YAML w języku C#](https://yaml.org/spec/)
- [Strona główna biblioteki YAMLDotNet](https://github.com/aaubry/YamlDotNet)
- [Porównanie YAML z innymi formatami danych](https://www.aonaware.com/yaml.html)