---
title:                "Trabalhando com YAML"
date:                  2024-01-19
html_title:           "Arduino: Trabalhando com YAML"
simple_title:         "Trabalhando com YAML"

category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

YAML é um formato de serialização de dados legível por humanos, usado frequentemente em arquivos de configuração e troca de dados entre linguagens. Programadores o utilizam pela simplicidade, legibilidade e por ser facilmente convertido para estruturas de dados como listas e dicionários.

## Como Fazer:

Para trabalhar com YAML em C#, você precisará de uma biblioteca como `YamlDotNet`. Veja como usar:

Instale o pacote via NuGet:

```
Install-Package YamlDotNet
```

Carregar um YAML a partir de uma string e converter para um objeto:

```C#
using System;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

public class Pessoa
{
    public string Nome { get; set; }
    public int Idade { get; set; }
}

// ...
var input = @"
nome: João
idade: 30
";

var deserializer = new DeserializerBuilder()
    .WithNamingConvention(CamelCaseNamingConvention.Instance)
    .Build();

Pessoa pessoa = deserializer.Deserialize<Pessoa>(input);
Console.WriteLine(pessoa.Nome); // Saída: João
```

Converter um objeto para YAML:

```C#
var serializer = new SerializerBuilder()
    .WithNamingConvention(CamelCaseNamingConvention.Instance)
    .Build();

string yaml = serializer.Serialize(new Pessoa { Nome = "Maria", Idade = 25 });
Console.WriteLine(yaml);
// Saída:
// nome: Maria
// idade: 25
```

## Mergulho Profundo

YAML, que significa "YAML Ain't Markup Language" (ou "YAML Não é Uma Linguagem de Marcação"), surgiu no início dos anos 2000 como uma alternativa ao XML. Embora seja menos verboso que o XML, YAML é comparavelmente tão expressivo quanto. Um bom conhecimento de YAML é essencial quando se está trabalhando com ferramentas de desenvolvimento modernas, como o Docker e Kubernetes. Alternativas ao YAML incluem JSON e XML, mas YAML é peculiarmente mais legível para configurações complexas. Implementá-lo em C# é fácil com o `YamlDotNet`, mas lembre-se que o tratamento do tipo de dados e campos ausentes deve ser cuidadoso para evitar erros de runtime.

## Veja Também

- Documentação Oficial do `YamlDotNet`: https://github.com/aaubry/YamlDotNet
- Especificação YAML: https://yaml.org/spec/1.2/spec.html
- Tutorial YAML: https://www.tutorialspoint.com/yaml/index.htm
- Guia JSON vs YAML: https://restfulapi.net/yaml-vs-json/
