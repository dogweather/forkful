---
title:                "Работа с TOML"
date:                  2024-01-29T00:05:10.773708-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c-sharp/working-with-toml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
TOML - это аббревиатура для Tom's Obvious, Minimal Language (Явный, Минималистичный Язык Тома) - формат файла конфигурации, который легко читается благодаря своей ясной семантике. Программисты используют его для конфигурационных файлов, упрощают обмен данными между системами и потому что он находит баланс между читаемостью для человека и возможностью разбора для машины.

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
