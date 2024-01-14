---
title:                "C#: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

##Чому

Отримання поточної дати є важливою функцією в багатьох програмуваннях, оскільки вона дозволяє користувачам відстежувати час та дату подій або виконувати певні дії на певні дні.

##Як

Для отримання поточної дати в C#, використовуйте клас DateTime. Спочатку необхідно ініціалізувати змінну типу DateTime та викликати метод Now() для отримання поточної дати. Нижче наведений приклад коду та його вихідного результату:

```C#
DateTime currentDate = DateTime.Now;
Console.WriteLine(currentDate);
```

Вихідний результат: 10/03/2021 13:27:32

Також можливо використовувати специфікатори формату для відображення дати у потрібному форматі. Наприклад:

```C#
DateTime currentDate = DateTime.Now;
Console.WriteLine(currentDate.ToString("dd/MM/yyyy"));
```

Вихідний результат: 03/10/2021

##Глибока занурення

Клас DateTime містить багато різноманітних методів для обробки дат та часу. Деякі з них є:

- Add(): додавання певних одиниць до поточної дати, наприклад додавання днів або місяців
- Subtract(): віднімання певних одиниць від поточної дати
- Equals(): перевірка на рівність двох дат
- Compare(): порівняння двох дат та повернення значення 1, -1 або 0 в залежності від того, яка дата більша
- IsLeapYear(): перевірка на те, чи є рік високосним

Не бійтесь експериментувати з методами класу DateTime та використовувати їх для своїх потреб. Знання роботи з датами допоможе зробити ваші програми більш ефективними та функціональними.

##Дивіться також

- [Клас DateTime в документації Microsoft](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [Робота з датами та часом в C#](https://docs.microsoft.com/en-us/dotnet/standard/datetime/)
- [10 корисних методів класу DateTime](https://www.c-sharpcorner.com/article/10-useful-methods-datetime-class-in-C-Sharp/)