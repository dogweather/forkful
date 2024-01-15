---
title:                "Перетворення рядка в нижній регістр"
html_title:           "C: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому
Зазвичай, в програмуванні є необхідність змінювати регістр символів у рядку для всіляких операцій. Наприклад, для порівняння двох рядків без урахування регістру або для підготовки вхідних даних для подальшої обробки.

## Як це зробити
Існує кілька способів перетворення регістру рядка на різні мови програмування, включаючи C. Але найпростішим і найбільш ефективним способом є використання стандартної функції `tolower()`.

```C
#include <stdio.h>
#include <ctype.h>

int main() {
    char str[] = "HELLO WORLD";
    
    // перетворення усіх символів на нижній регістр
    for (int i = 0; str[i] != '\0'; i++) {
        str[i] = tolower(str[i]);
    }
    
    printf("Результат: %s", str);
    
    return 0;
}

// Результат: hello world
```

Також варто зазначити, що функція `tolower()` працює лише з символами ASCII. Якщо ви маєте справу з юнікодом, необхідно використовувати інші методи.

## Глибоке занурення
В C існує такий термін, як "бібліотека стандартного вводу-виводу" (stdio), до якої належить функція `tolower()`. Вона містить набір корисних функцій для отримання входів від користувача, виведення даних тощо. І повертати символ в нижньому регістрі можна за допомогою коду ASCII. Значення, що повертає функція `tolower()` буде змінною типу `int`, яка відповість символу у нижньому регістрі. Ознайомитися з більш доступною інформацією можна на [цьому](https://www.geeksforgeeks.org/letters-conversion-lower-case-upper-case-using-c-programming/) сайті.

## Дивіться також
- [Функція `tolower()` у документації з мови програмування C](https://www.cplusplus.com/reference/cctype/tolower/)
- [Порівняння символів у рядку без урахування регістру](https://www.programiz.com/c-programming/library-function/string/strcmp/)
- [Юнікод в C](https://docs.microsoft.com/en-us/previous-versions/windows/desktop/legacy/dd374081(v=vs.85)?redirectedfrom=MSDN)