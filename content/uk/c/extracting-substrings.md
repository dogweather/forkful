---
title:                "Видобування підрядків"
html_title:           "C++: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що та навіщо?

Витягування підрядків у програмуванні - це процес отримання окремого рядка з більшого рядка. Ми це робимо для обробки та аналізу специфічних частин даних в більших рядках.

## Як це зробити:

Ось базовий приклад того, як ви можете витягнути підрядок в мові C за допомогою функції 'strncpy'.

```C
#include <string.h>
#include <stdio.h>

int main() {
    char source[] = "Вітаю, друже!";
    char destination[20];

    strncpy(destination, source, 5);
    destination[5] = '\0'; // закінчуємо рядок нульовим символом

    printf("%s\n", destination); // Виводить: Вітаю

    return 0;
}
```

## Поглиблено:

### Історичний контекст
Функція 'strncpy' є частиною стандартної бібліотеки C, що була інтегрована ще з часів розробки мови. 

### Альтернативи
На відміну від інших мов програмування, в С немає вбудованої функції для вилучення підрядків. Однак, ви можете використовувати 'strchr' та 'strrchr' для пошуку місця підрядка в рядку і потім використовувати 'strncmp' або 'strcpy' для копіювання підрядка.

### Деталі реалізації
' strncpy ' працює, копіюючи символи з одного рядка в інший до тих пір, поки вона не досягне кінця вихідного рядка або не досягне максимальної довжини підрядка.

## Дивіться також:

1. Функція 'strncpy': [link](https://www.cplusplus.com/reference/cstring/strncpy/)
2. Функції 'strchr' та 'strrchr': [link](http://www.cplusplus.com/reference/cstring/strrchr/)
3. Функція 'strncmp': [link](https://www.cplusplus.com/reference/cstring/strncmp/)
4. Створення власних функцій вилучення підрядків в C: [link](https://www.geeksforgeeks.org/c-program-to-extract-a-portion-of-string/) 

Програмування на мові C потребує розуміння як працювати з рядками та вилучати підрядки. Це важливий навик, який допоможе вам стати кращим програмістом.