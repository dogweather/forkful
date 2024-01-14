---
title:                "C#: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

##Чому

Робота з CSV є важливою для тих, хто працює з даними, оскільки цей формат даних зручний для зберігання та обміну інформацією. Незалежно від того, чи ви аналізуєте дані, створюєте звіти або імпортуєте дані в базу даних, робота з CSV є необхідною навичкою для ефективної роботи з даними.

##Як працювати з CSV

Для початку потрібно спочатку встановити бібліотеку CSVHelper через менеджер пакетів NuGet. Після цього, можна почати роботу з CSV файлами. Нижче наведений приклад коду на мові C#, який демонструє читання та запис даних з CSV файлу:

```C#
using (var reader = new StreamReader("file.csv"))
using (var csv = new CsvReader(reader, CultureInfo.InvariantCulture))
{
    var records = csv.GetRecords<Person>(); //зчитування даних з файлу у структуру Person
    
    foreach(var record in records)
    {
        //доступ до полів запису
        var name = record.Name;
        var age = record.Age;
        
        //виведення даних на екран
        Console.WriteLine($"Name: {name}, Age: {age}");
    }
}

//запис даних у CSV файл
var records = new List<Person>() 
{
    new Person { Name = "John", Age = 25 },
    new Person { Name = "Anna", Age = 30 },
    new Person { Name = "Mark", Age = 35 }
};

using (var writer = new StreamWriter("newFile.csv"))
using (var csv = new CsvWriter(writer, CultureInfo.InvariantCulture))
{
    csv.WriteRecords(records); //запис даних у файл
}
```

В результаті виконання цього коду, на екран будуть виведені дані з CSV файлу, а також створений новий файл з назвою "newFile.csv" з введеними даними.

##Глибше дослідження

При роботі з CSV важливо пам'ятати про деякі особливості цього формату. Наприклад, роздільник полів в CSS файлі може бути будь-яким символом, але найчастіше використовується кома. Також важливо бути уважним при роботі з текстовими поліми, оскільки деякі спеціальні символи, як наприклад крапка або кома, можуть спричинити проблеми при зчитуванні та записі даних.

Однією з головних переваг роботи з CSV є можливість швидко та ефективно обробляти велику кількість даних, оскільки цей формат займає мало місця та добре підходить для автоматизованої обробки даних.

##Додаткові ресурси

Для більш детального ознайомлення з роботою з CSV в C#, рекомендуємо переглянути наступні ресурси:

- [Офіційна документація CSVHelper](https://joshclose.github.io/CsvHelper/)
- [Стат