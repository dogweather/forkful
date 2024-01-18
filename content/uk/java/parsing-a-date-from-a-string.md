---
title:                "Розбір дати з рядка"
html_title:           "Java: Розбір дати з рядка"
simple_title:         "Розбір дати з рядка"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

Що & Чому?
Розбір дати з рядка означає перетворення рядка з датою у відповідну дату у форматі Java. Це корисний процес для програмістів, який дозволяє зробити роботу з датами більш простою та ефективною.

Як робити:
Для розбирання дати з рядка у Java, можна використати клас "SimpleDateFormat". Спочатку потрібно визначити шаблон для формату дати, наприклад "yyyy-MM-dd". Далі, можна застосувати метод "parse" до об'єкта "SimpleDateFormat", передавши рядок з датою як параметр. Результуючий об'єкт "Date" з буде містити дату у відповідному форматі.

```Java
SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd");
Date date = format.parse("2020-10-23");
System.out.println(date);
// Output: Fri Oct 23 00:00:00 EEST 2020
```

Задля зручності, клас "SimpleDateFormat" також має метод "format", який дозволяє перетворити об'єкт "Date" назад у рядок за вказаним шаблоном.

```Java
SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd");
Date date = new Date();
String dateString = format.format(date);
System.out.println(dateString);
// Output: 2020-10-23
```

Глибокий занурення:
Розбір дати з рядка є досить поширеною задачею для програмістів, оскільки дати є важливою частиною багатьох програм та систем. У Java є також інші альтернативи для роботи з датами, наприклад, клас "DateFormatter", який був створений в пізніших версіях Java 8.

Щоб зробити процес розбору дати більш точним і надійним, можна використовувати метод "setLenient" і передавати йому значення "false". Це дозволить виключити можливі помилки при неправильних форматах дати у рядку.

Дивіться також:
Для детальної інформації про клас "SimpleDateFormat" можна переглянути документацію на офіційному сайті Java. Також, на сайтах Stack Overflow та GitHub є багато варіантів коду для розбирання дати з рядка в Java.