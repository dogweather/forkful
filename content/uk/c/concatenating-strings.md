---
title:                "Злиття рядків"
html_title:           "C: Злиття рядків"
simple_title:         "Злиття рядків"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/concatenating-strings.md"
---

{{< edit_this_page >}}

Що і чому?
З'єднання рядків - це процес об'єднання двох або більше рядків в один. Програмісти використовують це для створення більш складних рядків, які містять інформацію з кількох джерел.

Як це зробити?
Нижче наведені кілька прикладів коду на мові C для з'єднання рядків. Результати виконання коду також показані у блоках ```C ... ```.
```
// Приклад 1:
#include <stdio.h>
int main() {
    char str1[20] = "Привіт ";
    char str2[20] = "Світ!";
    strcat(str1, str2);
    printf("%s", str1);
    return 0;
}
// Результат:
Привіт Світ!
```
```
// Приклад 2:
#include <stdio.h>
#include <string.h>
int main() {
    char str1[20] = "Це";
    char str2[20] = "приклад";
    char str3[20] = "рядка";
    strcat(str1, " ");
    strcat(str1, str2);
    strcat(str1, str3);
    printf("%s", str1);
    return 0;
}
// Результат:
Це приклад рядка
```
```
// Приклад 3:
#include <stdio.h>
int main() {
    char str1[20] = "Мова ";
    char str2[20] = "C";
    char str3[20] = " є потужною.";
    strcat(strcat(str1, str2), str3);
    printf("%s", str1);
    return 0;
}
// Результат:
Мова C є потужною.
```

Глибокі деталі:
Спосіб з'єднання рядків був запроваджений в мові C з метою полегшення будівлі більш складних рядків. Існують інші варіанти з'єднання рядків, такі як використання функції sprintf або використання знаку "+" на мовах програмування, які підтримують перевантаження операторів. У мові C немає обмежень на кількість рядків, які можна з'єднати, оскільки це залежить від доступної пам'яті.

Дивись також:
- [Documentation on string concatenation in C](https://en.cppreference.com/w/c/string/byte/strcat) - офіційна документація щодо з'єднання рядків у мові C
- [Wikipedia article on string concatenation](https://en.wikipedia.org/wiki/Concatenation_(computer_science)) - стаття на Вікіпедії про з'єднання рядків у комп'ютерних науках.