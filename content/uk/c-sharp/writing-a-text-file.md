---
title:                "C#: Написання текстового файлу"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Для чого
Запис текстових файлів є невід'ємною частиною програмування у C#. Це дозволяє зберігати дані в надійній формі та легко зчитувати їх для подальшої обробки.

## Як це зробити
Для запису текстового файлу у C# потрібно використовувати клас `StreamWriter`. Спочатку необхідно створити екземпляр цього класу з вказанням шляху до файлу, який потрібно створити або перезаписати. Далі, за допомогою методу `WriteLine()` можна записувати потрібні дані рядками. Наприклад:
```c#
StreamWriter writer = new StreamWriter("file.txt");
writer.WriteLine("Привіт світ!");
writer.WriteLine("Це перший рядок у моєму текстовому файлі.");
writer.Close();
```
У результаті ви отримаєте файл з назвою "file.txt", в якому будуть збережені два рядки тексту.

## Глибоке дослідження
Крім методу `WriteLine()`, клас `StreamWriter` має інші корисні методи для запису та форматування даних у текстовий файл. Наприклад, метод `Write()` дозволяє записувати дані без переведення на новий рядок, що створює можливість для створення таблиць та форматованого виводу. Також, за допомогою методу `Flush()` можна примусово записати дані з буфера у файл, що забезпечує більш швидкий запис.

## Дивіться також
- [Документація по класу StreamWriter](https://docs.microsoft.com/uk-ua/dotnet/api/system.io.streamwriter?view=netcore-3.1)
- [Робота з файлами у C#](https://www.c-sharpcorner.com/article/file-handling-in-C-Sharp/)
- [Форматування даних у C#](https://docs.microsoft.com/uk-ua/dotnet/standard/base-types/formatting-types)