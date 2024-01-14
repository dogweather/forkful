---
title:                "C#: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Чому

Якщо ви програміст із досвідом роботи з конфігураційними файлами, то ви, напевно, чули про формат YAML. YAML, або "YAML Ain't Markup Language" - це простий і зрозумілий спосіб зберігати дані у вигляді тексу. Проте, чому саме вам варто працювати з YAML? Давайте поглянемо на це питання разом.

YAML є дуже зручним форматом для зберігання інформації, так як він дозволяє створювати легкочитаємі файлів, які також можна легко редагувати у текстовому редакторі. Більше того, YAML підтримує структуру даних, яка дозволяє зберігати дані у вигляді хеш-таблиць та списків, що робить його дуже універсальним для різних цілей. Завдяки своїй простоті та зрозумілості, YAML є популярним форматом серед розробників програмного забезпечення.

## Як працювати з YAML

Зараз давайте перейдемо до практики і подивимось, як саме працює YAML у C#. Для початку, нам знадобиться бібліотека `YamlDotNet`, яка дозволяє зчитувати та записувати YAML файли. Для цього можна встановити пакет за допомогою менеджера пакетів **NuGet** або скористатися іншим зручним для вас способом.

Після того, як ми підключили `YamlDotNet`, можна почати робити роботу з YAML файлом. Для цього нам знадобиться створити об'єкт `YamlStream`, який дозволить нам зчитати дані з файлу. Ось приклад коду:

```C#
var yaml = @"
name: John
age: 25
hobbies:
- coding
- reading
- sports";

var input = new StringReader(yaml);
var yamlStream = new YamlStream();
yamlStream.Load(input);

var name = yamlStream.Documents[0].RootNode["name"].ToString();
var age = int.Parse(yamlStream.Documents[0].RootNode["age"].ToString());
var hobbies = (yamlStream.Documents[0].RootNode["hobbies"] as YamlSequenceNode).Children;

Console.WriteLine($"Name: {name}");
Console.WriteLine($"Age: {age}");
Console.WriteLine("Hobbies: ");
foreach (var hobby in hobbies)
{
    Console.WriteLine($"- {hobby}");
}
```

Вивід програми буде наступним:

```
Name: John
Age: 25
Hobbies:
- coding
- reading
- sports
```

Ми змогли зчитати дані з YAML файлу і розподілити їх за потрібними нам змінними. Також можна зберігати дані навіть більш складних структур, як наприклад хеш-таблиці або списки об'єктів. Це робить YAML дуже гнучким та підходящим для різних потреб.

## Заглиблення в YAML

Ми розгляну