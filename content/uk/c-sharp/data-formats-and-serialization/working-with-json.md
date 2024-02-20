---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:58.005425-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON (JavaScript Object\
  \ Notation) \u0432\u043A\u043B\u044E\u0447\u0430\u0454 \u0432 \u0441\u0435\u0431\
  \u0435 \u0430\u043D\u0430\u043B\u0456\u0437, \u0441\u0442\u0432\u043E\u0440\u0435\
  \u043D\u043D\u044F \u0442\u0430 \u0437\u0430\u043F\u0438\u0442\u0443\u0432\u0430\
  \u043D\u043D\u044F \u0434\u0430\u043D\u0438\u0445 JSON, \u0449\u043E \u0440\u043E\
  \u0431\u0438\u0442\u044C \u0446\u0435 \u043A\u0440\u0438\u0442\u0438\u0447\u043D\
  \u043E\u044E \u043D\u0430\u0432\u0438\u0447\u043A\u043E\u044E \u0434\u043B\u044F\
  \ \u0441\u0443\u0447\u0430\u0441\u043D\u043E\u0433\u043E\u2026"
lastmod: 2024-02-19 22:05:08.350175
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON (JavaScript Object Notation)\
  \ \u0432\u043A\u043B\u044E\u0447\u0430\u0454 \u0432 \u0441\u0435\u0431\u0435 \u0430\
  \u043D\u0430\u043B\u0456\u0437, \u0441\u0442\u0432\u043E\u0440\u0435\u043D\u043D\
  \u044F \u0442\u0430 \u0437\u0430\u043F\u0438\u0442\u0443\u0432\u0430\u043D\u043D\
  \u044F \u0434\u0430\u043D\u0438\u0445 JSON, \u0449\u043E \u0440\u043E\u0431\u0438\
  \u0442\u044C \u0446\u0435 \u043A\u0440\u0438\u0442\u0438\u0447\u043D\u043E\u044E\
  \ \u043D\u0430\u0432\u0438\u0447\u043A\u043E\u044E \u0434\u043B\u044F \u0441\u0443\
  \u0447\u0430\u0441\u043D\u043E\u0433\u043E\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON"
---

{{< edit_this_page >}}

## Що та чому?

Робота з JSON (JavaScript Object Notation) включає в себе аналіз, створення та запитування даних JSON, що робить це критичною навичкою для сучасного програмування. Цей формат обміну даними широко використовується у веб-сервісах та API завдяки його легкості для читання та незалежності від мови, роблячи його невід'ємним для програмістів на C#, які працюють над мережевими додатками або взаємодіють з даними, базованими на вебі.

## Як робити:

### Розбір Рядка JSON до Об'єкта

C# надає простір імен `System.Text.Json` для ефективної обробки JSON. Щоб розібрати рядок JSON до об'єкта C#, визначте клас, який відповідає структурі JSON, і використовуйте метод `JsonSerializer.Deserialize`.

```csharp
using System;
using System.Text.Json;

public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
}

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"Name\":\"John\", \"Age\":30}";
        Person person = JsonSerializer.Deserialize<Person>(jsonString);

        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}");
        // Вивід: Name: John, Age: 30
    }
}
```

### Генерація JSON з Об'єкта

Щоб перетворити об'єкт C# назад у рядок JSON, використайте метод `JsonSerializer.Serialize`.

```csharp
using System;
using System.Text.Json;

public class Program
{
    public static void Main()
    {
        Person person = new Person
        {
            Name = "Jane",
            Age = 25
        };

        string jsonString = JsonSerializer.Serialize(person);
        Console.WriteLine(jsonString);
        // Вивід: {"Name":"Jane","Age":25}
    }
}
```

### Використання Newtonsoft.Json

`Newtonsoft.Json` (або Json.NET) - популярна стороння бібліотека, яка пропонує більше гнучкості та опцій для серіалізації та десеріалізації JSON.

Щоб використовувати Json.NET, спочатку треба встановити пакет `Newtonsoft.Json` через NuGet. Після цього, ви можете десеріалізувати рядок JSON таким чином:

```csharp
using System;
using Newtonsoft.Json;

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"Name\":\"Mike\", \"Age\":22}";
        Person person = JsonConvert.DeserializeObject<Person>(jsonString);

        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}");
        // Вивід: Name: Mike, Age: 22
    }
}
```

Для генерації JSON з об'єкта за допомогою Json.NET:

```csharp
using System;
using Newtonsoft.Json;

public class Program
{
    public static void Main()
    {
        Person person = new Person
        {
            Name = "Ella",
            Age = 28
        };

        string jsonString = JsonConvert.SerializeObject(person);
        Console.WriteLine(jsonString);
        // Вивід: {"Name":"Ella","Age":28}
    }
}
```

Ці фрагменти пропонують швидкий старт для роботи з JSON у C#, демонструючи як вбудовані можливості `System.Text.Json`, так і розширені функції `Newtonsoft.Json`.
