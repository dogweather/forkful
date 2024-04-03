---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:24.328023-07:00
description: "\u041A\u0430\u043A: \u0414\u043B\u044F \u0440\u0430\u0431\u043E\u0442\
  \u044B \u0441 YAML \u0432 C# \u0432\u0430\u043C \u043F\u043E\u043D\u0430\u0434\u043E\
  \u0431\u0438\u0442\u0441\u044F \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\
  \u0430 YamlDotNet. \u0412\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u0443\u0441\
  \u0442\u0430\u043D\u043E\u0432\u0438\u0442\u044C \u0435\u0451 \u0447\u0435\u0440\
  \u0435\u0437 NuGet: `Install-Package YamlDotNet`. \u0421\u043D\u0430\u0447\u0430\
  \u043B\u0430 \u0434\u0430\u0432\u0430\u0439\u0442\u0435\u2026"
lastmod: '2024-03-13T22:44:45.093887-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043B\u044F \u0440\u0430\u0431\u043E\u0442\u044B \u0441 YAML \u0432\
  \ C# \u0432\u0430\u043C \u043F\u043E\u043D\u0430\u0434\u043E\u0431\u0438\u0442\u0441\
  \u044F \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0430 YamlDotNet."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
weight: 41
---

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
