---
title:                "Використання інтерактивної оболонки (REPL)"
date:                  2024-01-26T04:12:16.961216-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання інтерактивної оболонки (REPL)"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Що та Чому?
Інтерактивна оболонка або цикл Читання-Оцінювання-Друку (REPL) – це інструмент, що забезпечує середовище кодування в реальному часі для миттєвого тестування фрагментів коду. Програмісти використовують його для швидкого зворотного зв’язку під час розробки, навчання та відладки.

## Як:
C не має вбудованого REPL, але ви можете використовувати інструменти сторонніх розробників. Ось короткий огляд використання Cling, інтерпретатора C++, який також може обробляти код C:

```C
#include <stdio.h>

int main() {
    printf("Привіт, REPL світе!\n");
    return 0;
}
```

Вивід у REPL Cling:
```
[cling]$ .x yourscript.c
Привіт, REPL світе!
```

Cling виконує скрипт і миттєво виводить результат.

## Поглиблений огляд
REPL є стандартом у динамічних мовах, як-от Python або Ruby, але для компільованих мов, як-то C, вони менш поширені. Історично цикл компіляції-запуску-відладки не сприяв інтерактивному дослідженню. Інструменти на кшталт Cling та онлайн компілятори C пропонують досвід, схожий на REPL, обгортаючи ваш код C у середовищі C++.

Альтернативи для Cling включають інтерпретатори C, як CINT та Ch. Ці інструменти дозволяють швидку ітерацію, але можуть бути не підходящими для всіх сценаріїв розробки через обмеження продуктивності та підтримки складних функцій.

Реалізація REPL у компільованій мові включає компіляцію та виконання фрагментів коду на льоту, що є не простим завданням і може мати обмеження порівняно з повними можливостями мови.

## Див. також
- Cling: https://github.com/root-project/cling
- Онлайн C Компілятор та REPL: https://repl.it/languages/c
- CINT: http://root.cern.ch/drupal/content/cint
- Інтерпретатор Ch: http://www.softintegration.com/products/chstandard/