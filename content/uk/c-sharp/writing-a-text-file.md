---
title:                "C#: Написання текстового файлу"
simple_title:         "Написання текстового файлу"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Чому

Написання текстового файлу є важливим елементом в програмуванні, оскільки він дозволяє зберігати дані для подальшого використання в програмі. Використання файлів є корисним, коли дані мають бути доступними після зупинки програми.

## Як написати текстовий файл

Нижче показано приклад коду на мові C#, який демонструє створення та запис даних у текстовий файл:

```C#
using System; 
using System.IO; 

class Program 
{ 
    static void Main() 
    { 
        // створення текстового файлу для запису 
        StreamWriter file = new StreamWriter("myfile.txt"); 

        // запис даних у файл 
        file.WriteLine("Привіт, усім!"); 
        file.WriteLine("Це текстовий файл."); 

        // закриття файлу 
        file.Close(); 

        // повідомлення про успішний запис 
        Console.WriteLine("Дані було записано до файлу."); 
    } 
}
```

В результаті виконання цього коду у текстовому файлі "myfile.txt" будуть збережені наступні дані:

Привіт, усім!
Це текстовий файл.

## Глибше занурення

Крім запису даних у текстовий файл, також можна використовувати його для зчитування даних у програму. Для цього можна використовувати клас StreamReader. Доступ до файлу безпосередньо в програмі дозволяє легко працювати зі зчитаними даними та виконувати на них якісь додаткові дії.

Наведений нижче приклад демонструє зчитування даних з текстового файлу за допомогою класу StreamReader та виведення їх на консоль:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        // зчитування даних з файлу
        StreamReader file = new StreamReader("myfile.txt");

        // виведення даних на консоль
        string line;
        while ((line = file.ReadLine()) != null)
        {
            Console.WriteLine(line);
        }

        // закриття файлу
        file.Close();
    }
}
```

## Дивись також

- [Робота з файлами в C#](https://metanit.com/sharp/tutorial/8.1.php)
- [Клас StreamWriter](https://msdn.microsoft.com/ru-ru/library/system.io.streamwriter(v=vs.110).aspx)
- [Клас StreamReader](https://msdn.microsoft.com/ru-ru/library/system.io.streamreader(v=vs.110).aspx)