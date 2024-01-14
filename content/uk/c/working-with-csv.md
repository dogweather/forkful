---
title:                "C: Робота з CSV"
simple_title:         "Робота з CSV"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/working-with-csv.md"
---

{{< edit_this_page >}}

# Чому

Цікавитесь програмуванням на мові C? Хочете працювати з CSV? Нехай цей пост допоможе вам поповнити свої знання та розширити свої можливості.

# Як працювати з CSV в C

Використовувати CSV (Comma-Separated Values) дуже зручно для обробки табличних даних в програмах на C. Завантажте файли CSV в програму та виведіть їх в консоль або опрацюйте ці дані для своїх потреб.

### Приклад:

```
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main()
{
    FILE *fp;
    char line[1000];

    // відкриваємо файл з даними CSV
    fp = fopen("data.csv", "r");

    // зчитуємо та виводимо кожен рядок з файла
    while (fgets(line, sizeof(line), fp) != NULL)
    {
        // розділяємо рядок по комі
        char *token = strtok(line, ",");
        while (token != NULL)
        {
            // виводимо значення
            printf("%s ", token);
            // переходимо до наступного значення
            token = strtok(NULL, ",");
        }
        // перехід на наступний рядок
        printf("\n");
    }
    // закриваємо файл
    fclose(fp);

    return 0;
}
```

### Вихідний код для файлу `data.csv`

```
Name,Age,Country
John,25,USA
Anna,32,Ukraine
Jack,40,Canada
```

### Очікуваний результат

```
Name Age Country
John 25 USA
Anna 32 Ukraine
Jack 40 Canada
```

# Глибоке погруження

Тепер, коли ви знаєте як працювати з CSV в C, можна розвивати свої навички далі. Можна, наприклад, додати підтримку для обробки великих наборів даних, валідацію вхідних значень або повний функціонал для редагування та збереження CSV файлів. 

# Дивіться також

- [Розробка програм на мові C](https://prog.ua/c-1)
- [Робота з файлами у C](https://uk.wikibooks.org/wiki/%D0%A0%D0%BE%D0%B1%D0%BE%D1%82%D0%B0_%D0%B7_%D1%84%D0%B0%D0%B9%D0%BB%D0%B0%D0%BC%D0%B8%D1%81_%D1%83_C)
- [Ручна сторінка по роботі з CSV в C](https://linux.die.net/man/3/csv)

Я сподіваюся, що цей пост був корисним для вас. Запрошую до дослідження світу програмування та надзвичайних можливостей мови C. Щастя програмування!