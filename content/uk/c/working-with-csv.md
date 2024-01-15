---
title:                "Робота з CSV"
html_title:           "C: Робота з CSV"
simple_title:         "Робота з CSV"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/working-with-csv.md"
---

{{< edit_this_page >}}

## Чому

CSV (Comma-Separated Values) є одним з найбільш поширених форматів для зберігання і обміну даними. Його легко читати та писати, а також може бути використаним у більшості програмних мов. Працюючи з CSV у C, ви зможете забезпечити швидкий та ефективний обмін даними між різними програмами та системами.

## Як це зробити

Для роботи з CSV у C необхідно використовувати стандартну бібліотеку "stdio.h". Перш за все, необхідно відкрити файл у форматі "csv" за допомогою функції fopen(). Наступним кроком є прочитання та обробка даних у файлі. Для цього можна скористатися функціями fgets() та sscanf() для зчитування та збереження даних у зручний для подальшої обробки формат. Наприклад:

```C
FILE *fp;
fp = fopen("file.csv", "r");
if (fp == NULL) {
    perror("Error opening file");
    return 1;
}

char line[1024];
while (fgets(line, 1024, fp)) {
    char name[50], age[3];
    sscanf(line, "%[^,],%s\n", name, age);
    printf("Name: %s, Age: %s\n", name, age);
}
```

Цей код зчитує рядок з файлу у змінну "line" та розділяє його на дві частини за допомогою коми. Першу частину зберігає у змінну "name", а другу - у змінну "age", та виводить значення на екран.

## Глибокий дайв

При роботі з CSV варто пам'ятати про те, що дані можуть бути розділені не тільки комами, але й іншими символами, наприклад крапками чи табуляцією. Крім того, CSV може містити спеціальні символи, які слід враховувати при обробці даних. Для швидшої та зручнішої роботи з CSV у C можна використовувати сторонні бібліотеки, наприклад "csvparser".

## Дивись також

- [Використання стандартної бібліотеки "stdio.h"](https://www.programiz.com/c-programming/c-file-input-output)
- [Бібліотека "csvparser" для роботи з CSV у C](https://github.com/edrosten/libcsv)
- [Приклади роботи з CSV у C](https://www.codementor.io/@michaelsafyan/using-the-csv-parser-library-for-c-qj3p76qjg)