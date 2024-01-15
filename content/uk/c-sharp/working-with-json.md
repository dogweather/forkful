---
title:                "Робота з json"
html_title:           "C#: Робота з json"
simple_title:         "Робота з json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Чому
Цікавитесь програмуванням і хочете розширити свої знання? Користування форматом JSON допоможе вам ефективніше працювати з даними в вашому програмі. 

## Як користуватися JSON у C#
JSON (JavaScript Object Notation) є популярним форматом обміну даними, особливо веб-додатках. Для роботи з ним у C#, вам потрібно використовувати бібліотеку Newtonsoft.Json. Ось приклад коду для зчитування та запису даних у форматі JSON:

```C#
// Приклад об'єкта JSON
string json = @"{ 'name': 'John', 'age': 30, 'city': 'Kyiv' }";
// Зчитування об'єкта у форматі JSON у змінну
var person = JsonConvert.DeserializeObject<Person>(json);
// Виведення даних
Console.WriteLine($"Name: {person.name}");
Console.WriteLine($"Age: {person.age}");
Console.WriteLine($"City: {person.city}");
// Код у випадку, якщо об'єкта немає у масиві
string emptyJson = "[]";
var emptyList = JsonConvert.DeserializeObject<List<Person>>(emptyJson);
Console.WriteLine($"Number of persons in the list: {emptyList.Count}");
// Приклад створення об'єкта JSON
var newPerson = new Person()
{
    Name = "Anna",
    Age = 25,
    City = "Lviv"
};
// Перетворення у формат JSON
var newPersonJson = JsonConvert.SerializeObject(newPerson);
Console.WriteLine(newPersonJson);

// Код класу персона
public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
    public string City { get; set; }
}
```
Отриманий результат:
```
Name: John
Age: 30
City: Kyiv
Number of persons in the list: 0
{"name":"Anna","age":25,"city":"Lviv"}
```

## Поглиблене дослідження
У форматі JSON можна зберігати різні типи даних, такі як рядки, числа, булеві значення, масиви та об'єкти. Для цього використовується так звана "синтаксична цукерка", яка дозволяє зручно створювати та читати дані. У більш складних випадках, коли у об'єкта є багато полів, можна використовувати атрибути `[JsonProperty("коректне ім'я поля")]` для зручності конвертації даних.

## Дивіться також
- [Офіційна документація по бібліотеці Newtonsoft.Json](https://www.newtonsoft.com/json)
- [Відеоурок "Робота з JSON у C#"](https://www.youtube.com/watch?v=NXyZ-LhZcz0) 
- [Стаття "Робота з JSON в .NET за допомогою бібліотеки Newtonsoft.Json"](https://code.tutsplus.com/uk/tutorials/working-with-json-in-net--cms-31875)