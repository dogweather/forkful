---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:14.338536-07:00
description: "\u041A\u0430\u043A: \u0412 C \u043D\u0435\u0442 \u0432\u0441\u0442\u0440\
  \u043E\u0435\u043D\u043D\u043E\u0439 \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u043A\
  \u0438 \u0438\u0441\u043A\u043B\u044E\u0447\u0435\u043D\u0438\u0439, \u043A\u0430\
  \u043A \u0432 \u043D\u0435\u043A\u043E\u0442\u043E\u0440\u044B\u0445 \u0434\u0440\
  \u0443\u0433\u0438\u0445 \u044F\u0437\u044B\u043A\u0430\u0445. \u0412\u043C\u0435\
  \u0441\u0442\u043E \u044D\u0442\u043E\u0433\u043E \u043E\u043D\u0430 \u043E\u043F\
  \u0438\u0440\u0430\u0435\u0442\u0441\u044F \u043D\u0430 \u043D\u0435\u0441\u043A\
  \u043E\u043B\u044C\u043A\u043E \u0442\u0440\u0430\u0434\u0438\u0446\u0438\u043E\u043D\
  \u043D\u044B\u0445 \u0441\u0442\u0440\u0430\u0442\u0435\u0433\u0438\u0439 \u043E\
  \u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0438\u2026"
lastmod: '2024-03-13T22:44:45.931151-06:00'
model: gpt-4-0125-preview
summary: "\u0412 C \u043D\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u043D\
  \u043E\u0439 \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u043A\u0438 \u0438\u0441\
  \u043A\u043B\u044E\u0447\u0435\u043D\u0438\u0439, \u043A\u0430\u043A \u0432 \u043D\
  \u0435\u043A\u043E\u0442\u043E\u0440\u044B\u0445 \u0434\u0440\u0443\u0433\u0438\u0445\
  \ \u044F\u0437\u044B\u043A\u0430\u0445."
title: "\u041E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0430 \u043E\u0448\u0438\u0431\
  \u043E\u043A"
weight: 16
---

## Как:
В C нет встроенной поддержки исключений, как в некоторых других языках. Вместо этого она опирается на несколько традиционных стратегий обработки ошибок, таких как возвращение специальных значений из функций и установка глобальных переменных, например, `errno`.

**Возвращение специальных значений**

Функции могут указывать на ошибки, возвращая конкретное значение, которое вряд ли будет допустимым результатом. Вот пример с целыми числами:

```c
#include <stdio.h>

int inverse(int number, double *result) {
    if (number == 0) {
        return -1; // Ошибка
    } else {
        *result = 1.0 / number;
        return 0; // Успех
    }
}

int main() {
    double result;
    if (inverse(0, &result) < 0) {
        printf("Ошибка: Деление на ноль.\n");
    } else {
        printf("Обратное значение: %f\n", result);
    }
    
    return 0;
}
```

**Вывод:**
```
Ошибка: Деление на ноль.
```

**Проверка `errno`**

Для библиотечных функций, особенно тех, которые взаимодействуют с системой или ОС (например, I/O файлов), `errno` устанавливается, когда происходит ошибка. Для его использования подключите `errno.h` и проверяйте `errno` после предполагаемого сбоя:

```c
#include <stdio.h>
#include <errno.h>
#include <string.h>

int main() {
    FILE *file = fopen("nonexistent.txt", "r");
    if (file == NULL) {
        printf("Ошибка при открытии файла: %s\n", strerror(errno));
    }
    
    return 0;
}
```

**Вывод:**
```
Ошибка при открытии файла: Нет такого файла или каталога
```

## Погружение
Исторически минималистичный дизайн языка программирования C исключал встроенный механизм обработки исключений, что отражает его происхождение от программирования низкого уровня, где критически важны максимальная производительность и контроль на уровне «железа». Вместо этого, C принимает более ручной подход к обработке ошибок, который соответствует его философии предоставления программистам максимального контроля, даже ценой удобства.

Хотя этот подход хорошо сочетается с целями дизайна C, он также может привести к громоздкому коду проверки ошибок и потенциалу пропуска проверок ошибок, что современные языки решают с помощью структурированных механизмов обработки исключений. Например, исключения в языках, таких как Java или C#, позволяют централизованно обрабатывать ошибки, делая код чище и управление ошибками более простым. Однако, исключения вносят свои накладные расходы и сложности, которые могут быть неидеальными для системного программирования, где C блестит.

Несмотря на его недостатки, этот ручной подход к обработке ошибок в C повлиял на дизайн управления ошибками во многих других языках, предлагая модель, где явное указание на условия ошибок может привести к более предсказуемому и отлаживаемому коду. Для критических систем, где неудачи должны быть управляемыми с легкостью, парадигма обработки ошибок C в сочетании с современными лучшими практиками, такими как библиотеки обработки ошибок и конвенции, обеспечивает надежность и устойчивость.
