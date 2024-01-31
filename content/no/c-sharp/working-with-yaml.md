---
title:                "Arbeid med YAML"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med YAML"
simple_title:         "Arbeid med YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?

YAML, "YAML Ain't Markup Language", er et dataformat brukt for konfigurasjonsfiler og datautveksling. Programmerere bruker YAML for sin lesbarhet og enkelhet, noe som gjør det ideelt for innstillinger, deploy-script og å definere infrastruktur som kode.

## Hvordan:

For å jobbe med YAML i C#, trenger vi `YamlDotNet` biblioteket. Installer via NuGet:

```bash
Install-Package YamlDotNet
```

For å lese YAML:

```csharp
using YamlDotNet.Serialization;
using System.IO;

var deserializer = new DeserializerBuilder().Build();
var yamlStr = File.ReadAllText("config.yaml");
var config = deserializer.Deserialize<Dictionary<string, string>>(yamlStr);
Console.WriteLine(config["setting"]);
```

For å skrive til YAML:

```csharp
using YamlDotNet.Serialization;
using System.IO;

var serializer = new SerializerBuilder().Build();
var configToWrite = new Dictionary<string, string> {{"setting", "value"}};
using (var writer = File.CreateText("config.yaml"))
{
    serializer.Serialize(writer, configToWrite);
}
```

Eksempel `config.yaml` etter å ha kjørt koden:

```yaml
setting: value
```

## Dypdykk:

YAML ble introdusert i 2001 som et brukervennlig alternativ til XML. Det er også ofte sammenlignet med JSON, men er mer lesevennlig og skalerbart for komplekse konfigurasjoner. Mange verktøy som Docker, Kubernetes og Ansible bruker YAML. C# implementasjonen via YamlDotNet støtter både serialisering og deserialisering, og tilbyr fleksible API-er for å tilpasse prosessen.

## Se Også:

- YAML offisiell side: [https://yaml.org/](https://yaml.org/)
- Microsofts dokumentasjon om NuGet pakkebehandling: [https://docs.microsoft.com/en-us/nuget/](https://docs.microsoft.com/en-us/nuget/)
