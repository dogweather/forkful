---
title:                "C#: Робота з json"
simple_title:         "Робота з json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Чому

Написати програми на C# є цікавою та корисною навичкою для будь-якого українського програміста. Одним з корисних і важливих елементів програмування є робота з JSON. JSON - це формат даних, що дозволяє обмінюватися інформацією між різними програмами. Робота з JSON в C# дозволяє зручно та ефективно обмінюватися даними, що робить його важливим елементом в сучасному програмуванні.

## Як створити програму, що працює з JSON

Для роботи з JSON в C#, необхідно використовувати спеціальну бібліотеку - Newtonsoft.Json. Давайте подивимося на приклад створення програми, що зберігає дані у вигляді JSON файлу:

```C# 
// Підключення бібліотеки
using Newtonsoft.Json;

// Клас, що описує дані, які будуть зберігатися
public class Person
{
    public string Name { get; set; }
    public int Age{ get; set; }
}

// Основна функція програми
public static void Main()
{
    // Створюємо екземпляр класу Person з відповідними даними
    Person person = new Person()
    {
        Name = "Ivan",
        Age = 25
    };
    
    // Конвертуємо дані в JSON формат
    string json = JsonConvert.SerializeObject(person);
    
    // Записуємо дані у файл
    File.WriteAllText(@"C:\Person.json", json);
}

```

Після виконання програми, в папці C з'явиться новий файл з даними у форматі JSON:

``` 
{"Name":"Ivan","Age":25}
```

## Поглиблене вивчення

Робота з JSON може бути більш складною, ніж просто збереження даних у файлі. Наприклад, ми можемо отримати дані з вже існуючого JSON файлу та обробити їх у нашій програмі. Давайте переглянемо приклад розбору JSON даних.

```C# 
// Парсимо JSON файл та зберігаємо дані в змінну
string json = File.ReadAllText(@"C:\Person.json");

// Дані змінної перетворюємо у об'єкт
Person person = JsonConvert.DeserializeObject<Person>(json);

// Виводимо дані з об'єкту
Console.WriteLine("Name: " + person.Name);
Console.WriteLine("Age: " + person.Age);

```

В результаті, на екран виведеться:

```
Name: Ivan
Age: 25
```

Це лише загальний огляд роботи з JSON в C#. Якщо ви бажаєте більш детально ознайомитися з цією темою, рекомендуємо прочитати документацію по бібліотеці Newtonsoft.Json і додаткові приклади роботи з JSON.

## Дивіться також

- [Офіційна документація по бібліотец