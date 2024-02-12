---
title:                "Преобразование строки в нижний регистр"
date:                  2024-02-03T17:54:54.575508-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование строки в нижний регистр"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c/converting-a-string-to-lower-case.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Преобразование строки в нижний регистр в C заключается в преобразовании всех букв в верхнем регистре данной строки в соответствующие эквиваленты в нижнем регистре. Программисты часто выполняют эту операцию для стандартизации текстового ввода для сравнения, поисковых операций или просто для эстетической консистенции вывода.

## Как:

В C нет встроенной функции для непосредственного преобразования строки в нижний регистр, в отличие от некоторых высокоуровневых языков. Однако процесс может быть легко реализован с использованием функций стандартной библиотеки C. Ниже приведено пошаговое руководство и пример, иллюстрирующий, как преобразовать строку в нижний регистр.

```c
#include <stdio.h>
#include <ctype.h>

void toLowerCase(char *str) {
    while (*str) {
        *str = tolower(*str);
        str++;
    }
}

int main() {
    char text[] = "Hello, World!";
    printf("Исходная: %s\n", text);

    toLowerCase(text);
    printf("Нижний регистр: %s\n", text);

    return 0;
}
```

**Пример вывода:**

```
Исходная: Hello, World!
Нижний регистр: hello, world!
```

В этом примере функция `toLowerCase` проходит через каждый символ входной строки, преобразуя его в соответствующий эквивалент в нижнем регистре с использованием функции `tolower` из `ctype.h`. Модификация выполняется на месте, изменяя исходную строку.

## Глубокое погружение

Функция `tolower`, использованная в приведенном выше примере, является частью стандартной библиотеки C, в частности, находится в файле заголовка `ctype.h`. Она работает в зависимости от текущей локали, но для стандартной локали "C" она обрабатывает набор символов ASCII, где 'A' до 'Z' преобразуются в 'a' до 'z'.

Исторически обработка кодирования символов и преобразование регистра в C были тесно связаны с набором символов ASCII, что ограничивало его полезность в международных или локализованных приложениях, где распространены символы за пределами набора ASCII. Современные языки программирования могут предлагать встроенные методы строк для выполнения преобразования регистра, учитывая локаль и символы Unicode, чего не хватает в C нативно.

В сценариях, требующих обширной манипуляции с текстом, особенно с не-ASCII символами, программисты могут рассмотреть возможность использования библиотек, которые предлагают лучшую поддержку интернационализации, таких как ICU (International Components for Unicode). Однако для большинства приложений, работающих с текстом ASCII, демонстрируемый подход эффективен и прост. Это подчеркивает склонность C давать программистам контроль над манипуляцией данными, хотя и с большей затратой усилий по сравнению с языками более высокого уровня.