---
title:    "C#: Створення тимчасового файлу"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Чому:
Створення тимчасових файлів є важливою складовою частиною багатьох програм, оскільки вони забезпечують ефективне та безпечне зберігання тимчасових даних. Такі файли можуть бути використані для збереження зображень, текстових даних або навіть результатів роботи програми.

## Як створити тимчасовий файл:
Для створення тимчасового файлу у програмі на C# можна скористатися класом `System.IO.Path`, а саме методом `GetTempFileName()`, який автоматично генерує унікальне ім'я для файлу і повертає його повний шлях. Також можна використовувати клас `System.IO.File` та його метод `Create()`, який дозволяє вручну вказати шлях та ім'я для тимчасового файлу.

```C#
var tempFile1 = System.IO.Path.GetTempFileName();
Console.WriteLine(tempFile1); // Output: C:\Users\Username\AppData\Local\Temp\tmp554F.tmp

var tempFile2 = System.IO.Path.GetTempPath() + "MyTempFile.txt";
Console.WriteLine(tempFile2); // Output: C:\Users\Username\AppData\Local\Temp\MyTempFile.txt

var tempFile3 = System.IO.File.Create("C:\MyTempFolder\MyTempFile.txt");
Console.WriteLine(tempFile3); // Output: C:\MyTempFolder\MyTempFile.txt
```

## Глибше про створення тимчасових файлів:
При створенні тимчасового файлу через клас `System.IO.File`, потрібно пам'ятати, що програма самостійно не видалятиме його після завершення роботи. Тому рекомендується використовувати метод `System.IO.File.Delete()` для видалення файлу вручну або використовувати `System.IO.FileOptions.DeleteOnClose` при створенні файлу, щоб він автоматично видалився при закритті програми.

Ще одним важливим аспектом при створенні тимчасових файлів є їх безпека. Рекомендується використовувати унікальні імена для файлів і передавати їх через захищені з'єднання, щоб запобігти можливій зловживанню.

## Дивіться також:
- [Офіційна документація Microsoft про створення та роботу з тимчасовими файлами у C#](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.create?view=net-5.0)
- [Бібліотека .NET для роботи з тимчасовими файлами (TempFiles)](https://github.com/scottlamb/tempfiles)
- [Стаття на Habr про створення тимчасових файлів у C#](https://habr.com/ru/post/261437/)