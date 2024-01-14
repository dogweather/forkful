---
title:    "Java: Отримання поточної дати"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому

Отримання поточної дати є важливою задачею для багатьох програмістів, особливо якщо вони працюють з Java. Іноді нам потрібно використовувати поточну дату для логування подій або для розрахунків часу, а іноді просто для відображення поточної дати на екрані. Безумовно, отримання поточної дати є важливою і корисною функцією для багатьох програм.

## Як

Отримання поточної дати в Java дуже просте. Для цього ми можемо використовувати клас `LocalDate` з пакету `java.time`. Далі показано приклад коду та його вивід:

```Java
// імпортуємо клас LocalDate
import java.time.LocalDate;

// метод для отримання поточної дати
public static void getCurrentDate() {
    // отримуємо поточну дату з класу LocalDate
    LocalDate currentDate = LocalDate.now();

    // виводимо поточну дату на екран
    System.out.println("Поточна дата: " + currentDate);
}

// викликаємо метод для отримання поточної дати
getCurrentDate();
```

Вивід:

```
Поточна дата: 2021-11-19
```

Отримання поточної дати можливе також за допомогою класу `Calendar` з пакету `java.util`. Для цього потрібно використовувати метод `getInstance()` та методи `get()` для визначення поточної дати, місяця та року. Далі наведено приклад коду та його вивід:

```Java
// імпортуємо клас Calendar
import java.util.Calendar;

// метод для отримання поточної дати
public static void getCurrentDate() {
    // отримуємо поточну дату з класу Calendar
    Calendar currentDate = Calendar.getInstance();

    // визначаємо поточну дату, місяць та рік
    int day = currentDate.get(Calendar.DAY_OF_MONTH);
    int month = currentDate.get(Calendar.MONTH) + 1; // + 1, тому що значення місяця починаються з 0
    int year = currentDate.get(Calendar.YEAR);

    // виводимо поточну дату на екран
    System.out.println("Поточна дата: " + day + "." + month + "." + year);
}

// викликаємо метод для отримання поточної дати
getCurrentDate();
```

Вивід:

```
Поточна дата: 19.11.2021
```

## Deep Dive

У Java є багато різних методів та класів для отримання поточної дати. Найпоширенішими з них є `LocalDate` та `Calendar` які були розглянуті вище. Також є інші методи, такі як `SimpleDateFormat` та `DateTimeFormatter` для форматування дати. Більше інформації щодо отримання та роботи з поточною датою можна знайти у [документації Java](https