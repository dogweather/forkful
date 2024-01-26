---
title:                "Praca z yaml"
html_title:           "Arduino: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
YAML to format danych idealny do konfiguracji. Programiści używają go, bo jest czytelny dla ludzi i maszyn.

## How to: (Jak to zrobić:)
Aby pracować z YAML w C#, można użyć biblioteki `YamlDotNet`. Oto jak czytać i pisać pliki YAML:

```C#
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;
using System.IO;

var deserializer = new DeserializerBuilder()
    .WithNamingConvention(CamelCaseNamingConvention.Instance)
    .Build();

var myData = deserializer.Deserialize<MyClass>(File.ReadAllText("config.yaml"));

var serializer = new SerializerBuilder()
    .WithNamingConvention(CamelCaseNamingConvention.Instance)
    .Build();

string yaml = serializer.Serialize(myData);
File.WriteAllText("output.yaml", yaml);
```

Wyjście (`output.yaml`):
```yaml
myProperty: Wartość
```

## Deep Dive (Dogłębna analiza):
YAML powstał w 2001 roku jako skrót od "Yet Another Markup Language". Alternatywami dla YAML są JSON i XML, ale YAML jest zwykle bardziej czytelny. Podczas implementacji warto pamiętać, że YAML jest wrażliwy na wcięcia i wymaga uważnej kontroli struktury.

## See Also (Zobacz także):
- Dokumentacja `YamlDotNet`: https://github.com/aaubry/YamlDotNet
- Specyfikacja YAML: https://yaml.org/spec/
- Porównanie formatów danych: https://www.liquid-technologies.com/xml-vs-json-vs-yaml
