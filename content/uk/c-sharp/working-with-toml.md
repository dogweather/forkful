---
title:                "Робота з TOML"
date:                  2024-01-26T04:20:55.278424-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з TOML"

category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/working-with-toml.md"
---

{{< edit_this_page >}}

## Що і чому?
TOML це абревіатура для Tom's Obvious, Minimal Language, формат файлу конфігурації, який легко читати завдяки своїй чіткій семантиці. Програмісти використовують його для файлів конфігурації, спрощуючи обмін даними між системами, оскільки він знаходить баланс між читабельністю для людини та парсабельністю для машини.

## Як це зробити:
Спочатку встановіть парсер TOML, такий як `Tomlyn`. Використовуйте ваш менеджер пакунків:

```csharp
dotnet add package Tomlyn
```

Далі, парсіть файл TOML:

```csharp
using Tomlyn;
using Tomlyn.Model;
using System;

var tomlContent = @"
[owner]
name = 'Том Престон-Вернер'
dob = 1979-05-27T07:32:00Z";

var tomlTable = Toml.Parse(tomlContent).ToModel();

Console.WriteLine($"Власник: {tomlTable["owner"]["name"]}");
// Вивід:
// Власник: Том Престон-Вернер
```

Тепер створіть та запишіть TOML:

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
                { "name", "Том Престон-Вернер" },
                { "dob", "1979-05-27T07:32:00Z" }
            }
        }
    }
};

var tomlString = doc.ToString();
File.WriteAllText("config.toml", tomlString);
Console.WriteLine("TOML записано до config.toml");
// Вивід:
// TOML записано до config.toml
```

## Поглиблений вступ:
TOML був створений Томом Престон-Вернером, співзасновником GitHub, приблизно в 2013 році як реакція на обмеження існуючих форматів, таких як YAML та JSON у налаштуваннях конфігурації. Він спеціально розроблений для конфігів з сильним акцентом на те, щоб бути простим і недвозначним.

Альтернативні формати конфігурації включають YAML, JSON і XML. Однак, TOML виділяється тим, що є більш дружнім до людини, особливо для файлів конфігурації, де редагування вручну є загальноприйнятим. JSON, хоч і є універсальним, менш зручний для читання у складних конфігаціях, а XML є многослівним. YAML, хоч і схожий за читабельністю, може стати складним з великим використанням пробілів і має ризики безпеки з певним вмістом.

З точки зору імплементації, TOML зосереджений на чіткій відповідності до хеш-таблиці, що робить вилучення даних передбачуваним. З випуском версії 1.0.0, TOML закріпив свою специфікацію, покращуючи стабільність та підтримку інструментів.

## Дивіться також:
- Офіційний репозиторій TOML на GitHub і специфікації: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- Tomlyn, бібліотека .NET: [github.com/xoofx/Tomlyn](https://github.com/xoofx/Tomlyn)
