---
title:                "Робота з yaml"
html_title:           "C#: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Чому

В даній статті ми розглянемо, що таке YAML та чому це корисно для програмістів. Якщо ви працюєте зі структурованими даними, YAML буде незамінним інструментом для збереження та передачі інформації.

## Як

Якщо ви вже знайомі з мовою програмування C#, пропоную перейти до практичної частини та розглянути реалізацію YAML. Для цього нам знадобиться встановити пакет [YamlDotNet](https://github.com/aaubry/YamlDotNet) за допомогою [NuGet пакет менеджера](https://docs.microsoft.com/uk-ua/nuget/quickstart/install-and-use-a-package-in-visual-studio).

```C#
// Підключаємо необхідні бібліотеки
using YamlDotNet.Serialization;
using System.IO;

// Створюємо клас для збереження даних
public class User
{
  public string Name { get; set; }
  public int Age { get; set; }
  public bool IsMarried { get; set; }
}

// Створюємо екземпляр класу та заповнюємо його даними
User user = new User()
{
  Name = "John",
  Age = 25,
  IsMarried = false
};

// Ініціалізуємо серіалізатор та задаємо формат даних
Serializer serializer = new SerializerBuilder().Build();

// Серіалізуємо об'єкт в формат YAML
var yamlOutput = serializer.Serialize(user);

// Зберігаємо дані у файл
File.WriteAllText("data.yml", yamlOutput);

// Десеріалізуємо дані з файлу назад до об'єкту
var obj = serializer.Deserialize<User>(File.ReadAllText("data.yml"));

// Виводимо дані в консоль
Console.WriteLine(obj.Name); // "John"
Console.WriteLine(obj.Age); // 25
Console.WriteLine(obj.IsMarried); // False
```

## Глибше в деталі

YAML - це формат для збереження даних в людино-читабельному вигляді, що дозволяє представляти складні структури даних, такі як об'єкти та масиви. Це сильний інструмент для передачі та обміну даними між різними системами.

## Дивись також

- [Офіційна документація YamlDotNet](https://github.com/aaubry/YamlDotNet/wiki/Documentation)
- [Вступ до YAML в C#](https://www.codeproject.com/Articles/1214409/Beginners-Guide-to-yaml-in-csharp)