---
title:                "Видалення символів, що відповідають патерну"
html_title:           "C: Видалення символів, що відповідають патерну"
simple_title:         "Видалення символів, що відповідають патерну"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо це потрібно?
Видалення символів, що відповідають певному шаблону, це процес видалення символів із рядка відповідно до заданого шаблону. Програмісти роблять це, щоб очистити дані, покращити їх зручність для сприйняття чи для відповідності певному формату.

## Як це робити:
За допомогою бібліотеки `string.h` в C можна легко видалити символи за шаблоном. Ось приклад:

```C
#include <stdio.h>
#include <string.h>

void deleteChars(char *s, const char *d) {
  char *p = strpbrk(s, d);
  while (p != NULL) {
    memmove(p, p + 1, strlen(p));
    p = strpbrk(s, d);
  }
}

int main() {
  char s[] = "український текст";
  deleteChars(s, "аї");
  printf("%s\n", s);  // Output: "укрнський текст"
  return 0;
}
```

## Поглиблено
Видалення символів за шаблоном - це широко використовуваний прийом в програмуванні. У минулому розробники писали власні функції для цього, але з появою стандартних бібліотек C, вони отримали набагато простіші способи для цього.

Альтернативою є використання регулярних виразів, але вони можуть бути надмірно складними для простих завдань. Інший підхід - це використання змінної розміру масиву, але це може викликати збої пам'яті.

В основі функції `deleteChars` лежить функція `strpbrk`, яка знаходить перший з символів, що задані в даних, в рядку `s`. Після того, як символ знайдено, `memmove` видаляє цей символ, зсуваючи решту рядка на одну позицію назад.

## Дивитись також
1. [Документація C library function - strpbrk()](https://www.tutorialspoint.com/c_standard_library/c_function_strpbrk.htm)
2. [C library function - memmove()](https://www.tutorialspoint.com/c_standard_library/c_function_memmove.htm)
3. [C library function - strlen()](https://www.tutorialspoint.com/c_standard_library/c_function_strlen.htm)