---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:10.773708-07:00
description: "\u041A\u0430\u043A \u0441\u0434\u0435\u043B\u0430\u0442\u044C: \u0421\
  \u043D\u0430\u0447\u0430\u043B\u0430 \u0443\u0441\u0442\u0430\u043D\u043E\u0432\u0438\
  \u0442\u0435 \u043F\u0430\u0440\u0441\u0435\u0440 TOML, \u043D\u0430\u043F\u0440\
  \u0438\u043C\u0435\u0440, `Tomlyn`. \u0418\u0441\u043F\u043E\u043B\u044C\u0437\u0443\
  \u0439\u0442\u0435 \u0432\u0430\u0448 \u043C\u0435\u043D\u0435\u0434\u0436\u0435\
  \u0440 \u043F\u0430\u043A\u0435\u0442\u043E\u0432."
lastmod: '2024-03-13T22:44:45.098750-06:00'
model: gpt-4-0125-preview
summary: "\u0421\u043D\u0430\u0447\u0430\u043B\u0430 \u0443\u0441\u0442\u0430\u043D\
  \u043E\u0432\u0438\u0442\u0435 \u043F\u0430\u0440\u0441\u0435\u0440 TOML, \u043D\
  \u0430\u043F\u0440\u0438\u043C\u0435\u0440, `Tomlyn`."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 TOML"
weight: 39
---

## Как сделать:
Сначала установите парсер TOML, например, `Tomlyn`. Используйте ваш менеджер пакетов:

```csharp
dotnet add package Tomlyn
```

Затем разберите TOML файл:

```csharp
using Tomlyn;
using Tomlyn.Model;
using System;

var tomlContent = @"
[owner]
name = 'Tom Preston-Werner'
dob = 1979-05-27T07:32:00Z";

var tomlTable = Toml.Parse(tomlContent).ToModel();

Console.WriteLine($"Владелец: {tomlTable["owner"]["name"]}");
// Вывод:
// Владелец: Tom Preston-Werner
```

Теперь создайте и запишите TOML:

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
Console.WriteLine("TOML записан в config.toml");
// Вывод:
// TOML записан в config.toml
```

## Погружение:
TOML был создан Томом Престон-Вернером, сооснователем GitHub, примерно в 2013 году в качестве реакции на ограничения существующих форматов, таких как YAML и JSON, в настройках конфигурации. Он специально разработан для конфигов с сильным акцентом на простоту и однозначность.

Альтернативные форматы конфигурации включают YAML, JSON и XML. Однако TOML выделяется тем, что он более дружелюбный к пользователю, особенно для файлов конфигурации, где ручное редактирование является обычным делом. JSON, хотя и повсеместно используем, менее читаем для сложных конфигов, а XML является многословным. YAML, хотя и схож по читаемости, может стать сложным при интенсивном использовании пробелов и имеет риски безопасности с определенным содержанием.

С точки зрения реализации, TOML фокусируется на четком отображении в хеш-таблицу, обеспечивая предсказуемость извлечения данных. С выпуском версии 1.0.0 TOML утвердил свои спецификации, улучшив стабильность и поддержку инструментария.

## Смотрите также:
- Официальный репозиторий и спецификация TOML на GitHub: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- Tomlyn, библиотека для .NET: [github.com/xoofx/Tomlyn](https://github.com/xoofx/Tomlyn)
