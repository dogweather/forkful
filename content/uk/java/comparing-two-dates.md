---
title:    "Java: Порівняння двох дат."
keywords: ["Java"]
---

{{< edit_this_page >}}

## Почему

Перевернути часові точки, вставити часову та помножте це відображення на найбільший спільний дільник.

## Як

Для порівняння двох дат в Java використовуються методи класу Date або Calendar. Потрібно використати методи `compareTo ()` або `equals ()`, щоб порівняти дати. Наприклад, якщо ми хочемо порівняти дві дати `date1` та `date2`:

```
Java.util.Date date1 = new Java.util.Date (2019, 01, 01);
Java.util.Date date2 = new Java.util.Date (2018, 01, 01);

if (date1.equals (date2))
    System.out.println ("Дати однакові.");
else if (date1.after (date2))
    System.out.println ("Дата 1 пізніше за дату 2.");
else if (date1.before (date2))
    System.out.println ("Дата 1 раніше за дату 2.");
```

Вивід буде "Дата 1 пізніше за дату 2.", оскільки `date1` має більше рік (2019) ніж `date2` (2018).

## Глибоке занурення

При порівнянні дат варто враховувати часові зони та формувати дати відповідно до цих зон. Також потрібно бути уважними при обробці локальної дати та дати з суміжних зон. Необхідно також пам'ятати, що методи `equals ()` та `compareTo ()` повертають значення типу `boolean`, тобто `true` або `false`.

## Дивіться також

- [Java Docs - Клас Date](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java Docs - Клас Calendar](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Стаття про порівняння дат в Java](https://www.baeldung.com/java-compare-dates)
- [Стаття про роботу з часовими зонами в Java](https://www.baeldung.com/java-time-zone)