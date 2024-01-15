---
title:                "Робота з csv"
html_title:           "C#: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## Чому

Нещодавно створення та обробка файлів CSV стало дуже популярним у програмуванні, особливо в C#. Це дає можливість зберігати та обмінюватися даними легко і швидко. Цей стиль стає надзвичайно важливим для роботи зі структурованими даними із великою кількістю інформації.

## Як це зробити

Для початку нам потрібно імпортувати простір імен для роботи з CSV файлами. У нашому випадку це буде `using System.IO;`. Потім, нам потрібно визначити шлях до файлу CSV, з яким ми працюємо:

```C#
string path = "file.csv";
```

Після цього, ми можемо створити об'єкт `StreamReader`, який дозволить нам читати дані з файлу:

```C#
StreamReader sr = new StreamReader(path);
```

Тепер, за допомогою циклу `while`, ми можемо проходитися по кожному рядку файлу і оброблювати його:

```C#
while(!sr.EndOfStream)
{
    string line = sr.ReadLine();
    // тут можна виконати потрібні дії з кожним рядком
}
```

Для зручності, ми можемо також використовувати бібліотеку `CsvHelper`, яка дозволяє нам зчитати дані з файлу та обробляти їх вже у вигляді об'єктів. Для цього нам необхідно встановити бібліотеку через NuGet та підключити її до проекту:

```C#
CsvReader csvReader = new CsvReader(sr, CultureInfo.InvariantCulture);
var records = csvReader.GetRecords<User>().ToList();
```

Після цього, ми можемо працювати з нашими об'єктами та зберігати дані до нового файлу, зазначивши шлях до нього і використовуючи об'єкт `StreamWriter`:

```C#
string outputPath = "new_file.csv";
using(StreamWriter sw = new StreamWriter(outputPath, false, Encoding.UTF8))
{
    var csvWriter = new CsvWriter(sw, CultureInfo.InvariantCulture);
    csvWriter.WriteRecords(records); // зберігаємо дані до файлу
}
```

## Глибше занурення

У роботі з CSV є багато моментів, які можуть стати важливими, наприклад, робота з розділителями, кодуванням, назвами стовпців, тощо. Також, важливо врахувати особливості роботи з більшими обсягами даних, щоб уникнути помилок та зберегти продуктивність програми.

## Дивіться також

- [Документація Microsoft з роботи з CSV в C#](https://docs.microsoft.com/en-us/dotnet/api/system.io.filestream?view=netcore-3.1)
- [Туторіал на Blog Of LVA Technologies з роботи з CSV в C#](https://www.lva-tech.lv/2017/11/working-with-csv-in-csharp/)
- [Офіційна документ