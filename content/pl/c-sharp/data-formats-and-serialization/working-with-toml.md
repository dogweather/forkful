---
date: 2024-01-26 04:20:31.338600-07:00
description: "Jak to zrobi\u0107: Najpierw zainstaluj parser TOML, na przyk\u0142\
  ad `Tomlyn`. U\u017Cyj swojego mened\u017Cera pakiet\xF3w."
lastmod: '2024-03-13T22:44:35.432639-06:00'
model: gpt-4-0125-preview
summary: "Najpierw zainstaluj parser TOML, na przyk\u0142ad `Tomlyn`."
title: Praca z TOML
weight: 39
---

## Jak to zrobić:
Najpierw zainstaluj parser TOML, na przykład `Tomlyn`. Użyj swojego menedżera pakietów:

```csharp
dotnet add package Tomlyn
```

Następnie sparsuj plik TOML:

```csharp
using Tomlyn;
using Tomlyn.Model;
using System;

var tomlContent = @"
[owner]
name = 'Tom Preston-Werner'
dob = 1979-05-27T07:32:00Z";

var tomlTable = Toml.Parse(tomlContent).ToModel();

Console.WriteLine($"Właściciel: {tomlTable["owner"]["name"]}");
// Wyjście:
// Właściciel: Tom Preston-Werner
```

Teraz utwórz i zapisz TOML:

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
Console.WriteLine("TOML został zapisany do pliku config.toml");
// Wyjście:
// TOML został zapisany do pliku config.toml
```

## Dogłębna analiza:
TOML został stworzony przez Toma Preston-Wernera, współzałożyciela GitHuba, około 2013 roku jako reakcja na ograniczenia istniejących formatów, takich jak YAML i JSON, w ustawieniach konfiguracyjnych. Jest specjalnie zaprojektowany do konfiguracji z silnym naciskiem na prostotę i jednoznaczność.

Alternatywne formaty konfiguracyjne to YAML, JSON i XML. Jednak TOML wyróżnia się tym, że jest bardziej przyjazny dla człowieka, szczególnie w przypadku plików konfiguracyjnych, gdzie często edycja ręczna jest powszechna. JSON, mimo że wszechobecny, jest mniej czytelny dla skomplikowanych konfiguracji, a XML jest rozwlekły. YAML, choć podobny pod względem czytelności, może stać się skomplikowany przy intensywnym użyciu białych znaków i niesie ryzyko bezpieczeństwa przy pewnych treściach.

Pod względem implementacji, TOML koncentruje się na czystym mapowaniu do tablicy mieszającej, co sprawia, że ekstrakcja danych jest przewidywalna. Z wydaniem wersji 1.0.0 TOML ugruntował swoją specyfikację, poprawiając stabilność i wsparcie narzędzi.

## Zobacz także:
- Oficjalne repozytorium GitHub i specyfikacja TOML: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- Tomlyn, biblioteka .NET: [github.com/xoofx/Tomlyn](https://github.com/xoofx/Tomlyn)
