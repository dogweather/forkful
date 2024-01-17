---
title:                "Заголовок статті про комп'ютерне програмування: Прописування рядка."
html_title:           "C: Заголовок статті про комп'ютерне програмування: Прописування рядка."
simple_title:         "Заголовок статті про комп'ютерне програмування: Прописування рядка."
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?

В програмуванні існує концепція, відома як "капіталізація рядка". Це означає, що перша літера кожного слова в рядку буде перетворена на велику. Програмісти зазвичай роблять це для того, щоб полегшити читання коду та покращити його стиль.

## Як це зробити?

Нижче показані приклади коду на C, які демонструють, як зробити капіталізацію рядка. Для кращого розуміння, також включений вихідний текст до цих прикладів.

```C 
// Приклад 1:
#include <stdio.h>
#include <string.h>

int main(){
  char str[] = "hello world";
  int i;

  for(i = 0; i < strlen(str); i++){
    str[i] = toupper(str[i]);
  }

  printf("%s", str);
  return 0;
}

Вихідний текст: HELLO WORLD

// Приклад 2:
#include <stdio.h>
#include <ctype.h>

int main(){
  char str[] = "welcome to the galaxy";
  int i = 0;

  while(str[i] != '\0'){
    putchar(toupper(str[i]));
    i++;
  }
  return 0;
}

Вихідний текст: WELCOME TO THE GALAXY
```

## Поглиблене вивчення

Капіталізація рядків використовується з давніх часів, коли комп'ютери не мали можливості відображати різні розміри літер. Також існують інші способи змінити регістр рядка, такі як зміна регістра вручну за допомогою циклу або використання готових функцій відповідних бібліотек. Для капіталізації рядка використовуються функції toupper() та putchar().

## Дивіться також

- Функція toupper(): https://www.educative.io/edpresso/what-is-the-toupper-function-in-c
- Інші методи капіталізації: https://www.hackerearth.com/practice/notes/capitalize-a-string-in-cc/
- Більше про рядки: https://www.programiz.com/c-programming/c-strings