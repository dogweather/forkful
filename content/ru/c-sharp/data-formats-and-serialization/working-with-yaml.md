---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:24.328023-07:00
description: "YAML \u2014 \u044D\u0442\u043E \u0441\u0442\u0430\u043D\u0434\u0430\u0440\
  \u0442 \u0441\u0435\u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\u0438\u0438\
  \ \u0434\u0430\u043D\u043D\u044B\u0445, \u043E\u0440\u0438\u0435\u043D\u0442\u0438\
  \u0440\u043E\u0432\u0430\u043D\u043D\u044B\u0439 \u043D\u0430 \u0447\u0435\u043B\
  \u043E\u0432\u0435\u043A\u0430, \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\
  \u0435\u043C\u044B\u0439 \u0432 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\
  \u0438\u0440\u043E\u0432\u0430\u043D\u0438\u0438 \u0434\u043B\u044F \u0444\u0430\
  \u0439\u043B\u043E\u0432 \u043A\u043E\u043D\u0444\u0438\u0433\u0443\u0440\u0430\u0446\
  \u0438\u0438, \u0445\u0440\u0430\u043D\u0435\u043D\u0438\u044F \u0434\u0430\u043D\
  \u043D\u044B\u0445 \u0438 \u043C\u043D\u043E\u0433\u043E\u0433\u043E\u2026"
lastmod: 2024-02-19 22:05:04.049545
model: gpt-4-0125-preview
summary: "YAML \u2014 \u044D\u0442\u043E \u0441\u0442\u0430\u043D\u0434\u0430\u0440\
  \u0442 \u0441\u0435\u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\u0438\u0438\
  \ \u0434\u0430\u043D\u043D\u044B\u0445, \u043E\u0440\u0438\u0435\u043D\u0442\u0438\
  \u0440\u043E\u0432\u0430\u043D\u043D\u044B\u0439 \u043D\u0430 \u0447\u0435\u043B\
  \u043E\u0432\u0435\u043A\u0430, \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\
  \u0435\u043C\u044B\u0439 \u0432 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\
  \u0438\u0440\u043E\u0432\u0430\u043D\u0438\u0438 \u0434\u043B\u044F \u0444\u0430\
  \u0439\u043B\u043E\u0432 \u043A\u043E\u043D\u0444\u0438\u0433\u0443\u0440\u0430\u0446\
  \u0438\u0438, \u0445\u0440\u0430\u043D\u0435\u043D\u0438\u044F \u0434\u0430\u043D\
  \u043D\u044B\u0445 \u0438 \u043C\u043D\u043E\u0433\u043E\u0433\u043E\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
---

{{< edit_this_page >}}

## Что и почему?
YAML — это стандарт сериализации данных, ориентированный на человека, используемый в программировании для файлов конфигурации, хранения данных и многого другого. Программисты используют его за его читабельность и простоту в сложных приложениях и системах.

## Как:
Для работы с YAML в C# вам понадобится библиотека YamlDotNet. Вы можете установить её через NuGet: `Install-Package YamlDotNet`.

Сначала давайте сериализуем объект в строку YAML:

```C#
using System;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

public class Person {
    public string FirstName { get; set; }
    public string LastName { get; set; }
    public int Age { get; set; }
}

class Program {
    static void Main(string[] args) {
        var person = new Person {
            FirstName = "Jamie",
            LastName = "Smith",
            Age = 35
        };

        var serializer = new SerializerBuilder()
            .WithNamingConvention(CamelCaseNamingConvention.Instance)
            .Build();

        string yaml = serializer.Serialize(person);
        Console.WriteLine(yaml);
    }
}
```

Вывод:
```yaml
firstName: Jamie
lastName: Smith
age: 35
```

Теперь давайте прочитаем YAML-файл и десериализуем его:

```C#
using System;
using System.IO;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

class Program {
    static void Main(string[] args) {
        var yaml = @"
firstName: Jamie
lastName: Smith
age: 35
";
        
        var deserializer = new DeserializerBuilder()
            .WithNamingConvention(CamelCaseNamingConvention.Instance)
            .Build();
        
        Person person = deserializer.Deserialize<Person>(yaml);
        
        Console.WriteLine($"Привет, {person.FirstName} {person.LastName}!");
    }
}

public class Person {
    public string FirstName { get; set; }
    public string LastName { get; set; }
    public int Age { get; set; }
}
```

Вывод:
```
Привет, Jamie Smith!
```

## Подробнее
YAML, что означает "YAML Ain't Markup Language" (YAML — это не язык разметки), впервые был предложен в 2001 году, чтобы быть более читабельным, чем XML. Он широко используется в DevOps для конфигурации CI/CD пайплайнов, как, например, в файлах Docker Compose или манифестах развертывания Kubernetes. JSON является надмножеством YAML, что означает, что файлы JSON также являются валидными YAML. С точки зрения реализации, для разбора YAML в C# требуется библиотека, такая как YamlDotNet, поскольку нет нативной поддержки.

## См. также
- [GitHub-репозиторий YamlDotNet](https://github.com/aaubry/YamlDotNet)
- [Официальный веб-сайт YAML](https://yaml.org)
- [Спецификация YAML](https://yaml.org/spec/1.2/spec.html)
