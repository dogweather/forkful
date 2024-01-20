---
title:                "Читання текстового файлу"
html_title:           "Arduino: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що й навіщо?
Читання текстового файлу - це процес витягування даних з файлу, збереженого на вашому комп'ютері. Програмісти це роблять, щоб обробляти збережені дані, наприклад, конфігурації, дані користувача або просто заради обміну інформацією між різними частинами програми.

## Як це зробити:
Ось приклад C коду, що демонструє, як читати текстовий файл.

```C
#include <stdio.h>

int main() {
   FILE *file;
   char c;

   file = fopen("file.txt", "r");
   if (file) {
      while ((c=getc(file)) != EOF) {
         putchar(c);
      }
      fclose(file);
   }
   return 0;
}
```

Примітка: `EOF` - це спеціальний символ, що вказує на кінець файлу.

## Поглиблений розділ
Читання файлів було важливою частиною програмування від самого початку його існування, що дозволило побудувати багато алгоритмів і систем, на яких ми покладаємося сьогодні. Багато інших мов програмування, наприклад, Python або Java, пропонують власні засоби для роботи з файлами. Однак, C пропонує дуже простий і гнучкий підхід.

Особливість файлової системи в C полягає у тому, що вона передає всі деталі роботи з файлом на користувача. Але саме завдяки цьому, програміст має повний контроль над файлом і може здійснювати з ним більш детальні операції.

## Дивіться також
1. Документація GNU C Library по fopen: https://www.gnu.org/software/libc/manual/html_node/Opening-Streams.html
2. Стандарт ядра Linux по роботі з файлами: https://www.kernel.org/doc/html/latest/filesystems/vfs.html
3. Курс Coursera "Programming for Everybody (Getting Started with Python)": https://www.coursera.org/learn/python.