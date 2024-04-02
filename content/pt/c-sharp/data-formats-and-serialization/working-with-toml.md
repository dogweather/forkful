---
date: 2024-01-26 04:20:21.922184-07:00
description: "TOML \xE9 uma sigla para Tom's Obvious, Minimal Language, um formato\
  \ de arquivo de configura\xE7\xE3o que \xE9 f\xE1cil de ler devido \xE0 sua clareza\
  \ sem\xE2ntica.\u2026"
lastmod: '2024-03-13T22:44:46.607284-06:00'
model: gpt-4-0125-preview
summary: "TOML \xE9 uma sigla para Tom's Obvious, Minimal Language, um formato de\
  \ arquivo de configura\xE7\xE3o que \xE9 f\xE1cil de ler devido \xE0 sua clareza\
  \ sem\xE2ntica.\u2026"
title: Trabalhando com TOML
weight: 39
---

## O Que e Por Que?
TOML é uma sigla para Tom's Obvious, Minimal Language, um formato de arquivo de configuração que é fácil de ler devido à sua clareza semântica. Programadores o utilizam para arquivos de configuração, simplificando a troca de dados entre sistemas, e porque ele alcança um equilíbrio entre a legibilidade humana e a capacidade de análise por máquinas.

## Como Fazer:
Primeiro, instale um analisador de TOML como o `Tomlyn`. Use seu gerenciador de pacotes:

```csharp
dotnet add package Tomlyn
```

Em seguida, analise um arquivo TOML:

```csharp
using Tomlyn;
using Tomlyn.Model;
using System;

var tomlContent = @"
[owner]
name = 'Tom Preston-Werner'
dob = 1979-05-27T07:32:00Z";

var tomlTable = Toml.Parse(tomlContent).ToModel();

Console.WriteLine($"Proprietário: {tomlTable["owner"]["name"]}");
// Saída:
// Proprietário: Tom Preston-Werner
```

Agora, crie e escreva TOML:

```csharp
using Tomlyn;
using Tomlyn.Syntax;
using System;
using System.IO;

var doc = new DocumentSyntax
{
    Tables =
    {
        new TableSyntax("owner")
        {
            Items =
            {
                { "name", "Tom Preston-Werner" },
                { "dob", "1979-05-27T07:32:00Z" }
            }
        }
    }
};

var tomlString = doc.ToString();
File.WriteAllText("config.toml", tomlString);
Console.WriteLine("TOML escrito em config.toml");
// Saída:
// TOML escrito em config.toml
```

## Aprofundamento:
O TOML foi criado por Tom Preston-Werner, co-fundador do GitHub, por volta de 2013 como uma reação às limitações de formatos existentes como YAML e JSON em configurações. Ele é especificamente projetado para configurações com uma forte ênfase em ser direto e inequívoco.

Formatos alternativos de configuração incluem YAML, JSON e XML. No entanto, TOML se destaca por ser mais amigável para humanos, particularmente para arquivos de configuração onde a edição manual é comum. JSON, embora onipresente, é menos legível para configurações complexas, e XML é verboso. YAML, embora similar em legibilidade, pode se complicar pelo uso intenso de espaços em branco e apresenta riscos de segurança com certos conteúdos.

Em termos de implementação, o TOML foca em mapear de forma limpa para uma tabela hash, tornando a extração de dados previsível. Com o lançamento da versão 1.0.0, o TOML solidificou sua especificação, melhorando a estabilidade e o suporte de ferramentas.

## Veja Também:
- Repositório Oficial e Especificação do TOML no GitHub: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- Tomlyn, a biblioteca .NET: [github.com/xoofx/Tomlyn](https://github.com/xoofx/Tomlyn)
