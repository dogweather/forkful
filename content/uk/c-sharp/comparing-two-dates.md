---
title:                "C#: Порівняння двох дат"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

##Чому

Зацікавлення у порівнянні двох дат може виникнути з різних причин. Наприклад, ви можете потребувати визначити, яка з двох дат є більш новою або якщо вони співпадають. Також це може бути корисно у випадку, коли потрібно сортувати дані за датою.

##Як

Для порівняння дат у C# використовується клас `DateTime`. Для початку, ми можемо створити дві змінні цього класу, які будуть містити наші дати. Наприклад:

```C#
DateTime date1 = new DateTime(2020, 11, 15);
DateTime date2 = new DateTime(2021, 5, 20);
```

Далі ми можемо використати метод `Compare` для порівняння цих двох дат. Він повертає цілочисельне значення, яке показує, чи одна дата менша, більша або рівна іншій. Наприклад:

```C#
int result = DateTime.Compare(date1, date2);
```

Значення `result` буде -1, оскільки дата 15 листопада 2020 року є меншою, ніж 20 травня 2021 року.

##Поглиблене дослідження

Це лише найпростіший спосіб порівняти дві дати. У C# є багато інших методів та функцій, які можуть допомогти у цьому процесі. Наприклад, методи `Equals`, `Add`, `Subtract` та багато інших. Також, ви можете використати різні формати дат для отримання різних результатів.

Наведений приклад не враховує часові зони, що також може впливати на порівняння дат. Для цього варто врахувати методи `ToUniversalTime` та `ToLocalTime`.

##Подивіться також

- [DateTime.Compare Method (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compare?view=net-5.0)
- [DateTime Methods in C# (C# Corner)](https://www.c-sharpcorner.com/blogs/datetime-method-in-c-sharp-programming1)
- [Different date formats in C# (Stack Overflow)](https://stackoverflow.com/questions/11452588/different-date-formats-in-c-sharp)