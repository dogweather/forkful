---
title:                "Інтерполяція рядка"
html_title:           "C: Інтерполяція рядка"
simple_title:         "Інтерполяція рядка"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Що та чому?

Інтерполяція рядків - це процес заміни певних виразів у рядку змінними значеннями. Це допомагає програмістам зробити свій код більш динамічним та зручним для роботи з великими обсягами даних.

## Як це зробити:

```C
#include <stdio.h>

int main()
{
    char name[] = "John";
    int age = 25;
    printf("Привіт, мене звати %s та мені %d років.", name, age);
    return 0;
}
```
Вихідним результатом буде:
```Привіт, мене звати John та мені 25 років.```

## Глибший аналіз:

Інтерполяція рядків була вперше використана у мові програмування PL/I в 1960-х роках та пізніше з'явилася і в інших мовах програмування, таких як C і Java. Існують альтернативні способи заміни виразів у рядку, такі як використання функцій «заміни» або маніпулювання рядками. Використання інтерполяції рядків було особливо корисним у веб-програмуванні, де потрібно поєднати строки з даними з бази даних.

## Дивись також:

- Офіційна документація мови C: [https://www.iso.org/standard/74528.html](https://www.iso.org/standard/74528.html)
- Стаття про інтерполяцію рядків у Java: [https://www.baeldung.com/java-string-interpolation](https://www.baeldung.com/java-string-interpolation)
- Приклади використання інтерполяції рядків у веб-програмуванні: [https://www.tutorialspoint.com/interpolation-in-jsp](https://www.tutorialspoint.com/interpolation-in-jsp)