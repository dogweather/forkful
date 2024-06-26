---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:03:22.613917-07:00
description: "\u041A\u0430\u043A \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\
  \u0430\u0442\u044C: \u0417\u0430\u043F\u0443\u0441\u0442\u0438\u0442\u0435 REPL\
  \ \u0432 \u0432\u0430\u0448\u0435\u0439 \u0441\u0440\u0435\u0434\u0435 C# \u0438\
  \u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044F \u043E\u043A\u043D\u043E C# Interactive\
  \ \u0438\u043B\u0438 \u0432\u044B\u043F\u043E\u043B\u043D\u0438\u0442\u0435 `dotnet-script`\
  \ \u0432 \u0432\u0430\u0448\u0435\u043C \u0442\u0435\u0440\u043C\u0438\u043D\u0430\
  \u043B\u0435. \u0412\u043E\u0442 \u043D\u0435\u0431\u043E\u043B\u044C\u0448\u043E\
  \u0439 \u043F\u0440\u0438\u043C\u0435\u0440\u2026"
lastmod: '2024-03-13T22:44:45.059489-06:00'
model: gpt-4-0125-preview
summary: "\u0417\u0430\u043F\u0443\u0441\u0442\u0438\u0442\u0435 REPL \u0432 \u0432\
  \u0430\u0448\u0435\u0439 \u0441\u0440\u0435\u0434\u0435 C# \u0438\u0441\u043F\u043E\
  \u043B\u044C\u0437\u0443\u044F \u043E\u043A\u043D\u043E C# Interactive \u0438\u043B\
  \u0438 \u0432\u044B\u043F\u043E\u043B\u043D\u0438\u0442\u0435 `dotnet-script` \u0432\
  \ \u0432\u0430\u0448\u0435\u043C \u0442\u0435\u0440\u043C\u0438\u043D\u0430\u043B\
  \u0435."
title: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\
  \ \u0438\u043D\u0442\u0435\u0440\u0430\u043A\u0442\u0438\u0432\u043D\u043E\u0439\
  \ \u043E\u0431\u043E\u043B\u043E\u0447\u043A\u0438 (REPL)"
weight: 34
---

## Как использовать:
Запустите REPL в вашей среде C# используя окно C# Interactive или выполните `dotnet-script` в вашем терминале. Вот небольшой пример использования:

```csharp
> var greeting = "Привет, REPL!";
> Console.WriteLine(greeting);
Привет, REPL!
>
```

Вы моментально получаете обратную связь. Нет шагов компиляции и запуска. Просто кодируйте и смотрите результат.

## Погружение
REPL пришёл к нам из Lisp и нашёл свое применение в современных языках, особенно процветая в динамичных, таких как Python. С C#, Roslyn сблизил REPL с разработчиками. `csi` для Roslyn и `dotnet-script` для .NET Core — надежные варианты. Более глубокое погружение: они выполняют код построчно, а не все сразу, что является отличительной моделью исполнения по сравнению с типичными приложениями на C#. Это влияет на сохранение состояния между выполнениями и область видимости переменных.

Окно C# Interactive в Visual Studio — это REPL, работающий на основе Roslyn. Он имеет поддержку Intellisense, множественные ссылки и поддержку пакетов NuGet. Довольно большой шаг вперёд по сравнению с ранними экспериментами в командной строке.

Для альтернативных языков, Python использует `IDLE`, JavaScript имеет REPL в Node.js, а F# поставляется с `F# Interactive`. Каждый из них способствует мгновенной обратной связи, что бесценно для тестирования небольших фрагментов кода или понимания особенностей языка.

## Смотрите также
- [REPL `dotnet-script` для .NET Core](https://github.com/filipw/dotnet-script)
