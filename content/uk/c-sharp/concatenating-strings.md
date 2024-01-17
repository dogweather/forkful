---
title:                "З'єднання рядків"
html_title:           "C#: З'єднання рядків"
simple_title:         "З'єднання рядків"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що і чому?

Конкатенацію рядків у програмуванні можна описати як об'єднання двох або більше рядків у один. Це часто використовується для створення більш складних рядків з існуючих частин. Програмісти зазвичай використовують конкатенацію рядків, щоб створити динамічні повідомлення, форматувати вивід або з'єднати дані з різних джерел.

## Як робити:

```C#
// Приклад коду для конкатенації двох рядків у C#:
string firstName = "Вася";
string lastName = "Пупкін";
string fullName = firstName + " " + lastName;

// Результат:
// fullName = "Вася Пупкін"
```

```C#
// Приклад коду для конкатенації декількох рядків у C#:
string language = "C#";
string version = "9";
string welcomeMessage = "Ласкаво просимо до " + language + " " + version + "!";

// Результат:
// welcomeMessage = "Ласкаво просимо до C# 9!"
```

## Глибше процес:

У процесі розробки мови програмування, конкатенація рядків відіграє важливу роль у формуванні більш складних повідомлень та даних. Альтернативними методами конкатенації є використання бібліотечних функцій або використання форматованих рядків. У C#, рядки можуть бути конкатеновані за допомогою оператора `+` або за допомогою методу `Concat()` з класу `String`.

## Також перегляньте:

- [Оператор конкатенації](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/concatenation-operator)
- [Клас System.String](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=net-5.0)
- [Форматовані рядки у C#](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated#simple-interpolation)
- [StringBuilder клас](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=net-5.0)
- [Конкатенація рядків у JavaScript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/concat)