---
title:                "Работа с YAML"
aliases:
- /ru/c-sharp/working-with-yaml.md
date:                  2024-01-29T00:05:24.328023-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c-sharp/working-with-yaml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
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
