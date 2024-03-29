---
date: 2024-01-26 04:20:55.278424-07:00
description: "TOML \u0446\u0435 \u0430\u0431\u0440\u0435\u0432\u0456\u0430\u0442\u0443\
  \u0440\u0430 \u0434\u043B\u044F Tom's Obvious, Minimal Language, \u0444\u043E\u0440\
  \u043C\u0430\u0442 \u0444\u0430\u0439\u043B\u0443 \u043A\u043E\u043D\u0444\u0456\
  \u0433\u0443\u0440\u0430\u0446\u0456\u0457, \u044F\u043A\u0438\u0439 \u043B\u0435\
  \u0433\u043A\u043E \u0447\u0438\u0442\u0430\u0442\u0438 \u0437\u0430\u0432\u0434\
  \u044F\u043A\u0438 \u0441\u0432\u043E\u0457\u0439 \u0447\u0456\u0442\u043A\u0456\
  \u0439 \u0441\u0435\u043C\u0430\u043D\u0442\u0438\u0446\u0456. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438\u2026"
lastmod: '2024-03-13T22:44:49.325908-06:00'
model: gpt-4-0125-preview
summary: "TOML \u0446\u0435 \u0430\u0431\u0440\u0435\u0432\u0456\u0430\u0442\u0443\
  \u0440\u0430 \u0434\u043B\u044F Tom's Obvious, Minimal Language, \u0444\u043E\u0440\
  \u043C\u0430\u0442 \u0444\u0430\u0439\u043B\u0443 \u043A\u043E\u043D\u0444\u0456\
  \u0433\u0443\u0440\u0430\u0446\u0456\u0457, \u044F\u043A\u0438\u0439 \u043B\u0435\
  \u0433\u043A\u043E \u0447\u0438\u0442\u0430\u0442\u0438 \u0437\u0430\u0432\u0434\
  \u044F\u043A\u0438 \u0441\u0432\u043E\u0457\u0439 \u0447\u0456\u0442\u043A\u0456\
  \u0439 \u0441\u0435\u043C\u0430\u043D\u0442\u0438\u0446\u0456. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 TOML"
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
