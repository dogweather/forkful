---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:58.248462-07:00
description: "Como fazer: C# n\xE3o tem suporte integrado para YAML, mas voc\xEA pode\
  \ facilmente trabalhar com YAML utilizando bibliotecas de terceiros, como *YamlDotNet*.\u2026"
lastmod: '2024-03-13T22:44:46.604100-06:00'
model: gpt-4-0125-preview
summary: "C# n\xE3o tem suporte integrado para YAML, mas voc\xEA pode facilmente trabalhar\
  \ com YAML utilizando bibliotecas de terceiros, como *YamlDotNet*."
title: Trabalhando com YAML
weight: 41
---

## Como fazer:
C# não tem suporte integrado para YAML, mas você pode facilmente trabalhar com YAML utilizando bibliotecas de terceiros, como *YamlDotNet*. Primeiro, você precisa instalar o pacote YamlDotNet:

```bash
Install-Package YamlDotNet -Version 11.2.1
```

### Lendo YAML:
Imagine que você tenha um arquivo YAML `config.yaml` com o seguinte conteúdo:
```yaml
appSettings:
  name: MyApp
  version: 1.0.0
```

Você pode ler e analisar este arquivo YAML em C# assim:
```csharp
using System;
using System.IO;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

public class AppConfig
{
    public AppSettings appSettings { get; set; }
}

public class AppSettings
{
    public string name { get; set; }
    public string version { get; set; }
}

class Program
{
    static void Main(string[] args)
    {
        var yaml = File.ReadAllText("config.yaml");
        var deserializer = new DeserializerBuilder()
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // Ajuste a convenção de nomes conforme necessário
            .Build();

        var config = deserializer.Deserialize<AppConfig>(yaml);

        Console.WriteLine($"Nome: {config.appSettings.name}, Versão: {config.appSettings.version}");
    }
}
```
**Saída de Exemplo:**
```
Nome: MyApp, Versão: 1.0.0
```

### Escrevendo YAML:
Para escrever dados em um arquivo YAML, use a classe `Serializer` do YamlDotNet. Aqui está como você serializa um objeto de volta para YAML:

```csharp
using System;
using System.IO;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

class Program
{
    static void Main(string[] args)
    {
        var config = new AppConfig
        {
            appSettings = new AppSettings
            {
                name = "MyApp",
                version = "2.0.0"
            }
        };

        var serializer = new SerializerBuilder()
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // Adjust the naming convention accordingly
            .Build();

        var yaml = serializer.Serialize(config);
        File.WriteAllText("updatedConfig.yaml", yaml);

        Console.WriteLine(yaml);
    }
}
```
**Saída de Exemplo:**
```yaml
appSettings:
  name: MyApp
  version: 2.0.0
```

Esta abordagem direta demonstra como trabalhar de maneira eficiente com YAML em seus projetos C#, facilitando a leitura e a escrita em arquivos YAML usando a biblioteca YamlDotNet.
