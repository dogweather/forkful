---
title:                "Пошук та заміна тексту"
html_title:           "C++: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що і чому?
Пошук та заміна тексту - це процес виявлення вказаного шматка тексту у скрипті або файли чи строках строках та заміна його на новий текст. Програмісти роблять це для модифікації даних, виправлення помилок або оновлення інформації.

## Як це зробити:
Ось поданий нижче код, який виконує пошук та заміну тексту в C.

```C
#include <stdio.h>
#include <string.h>

void search_replace(char *str, char *old_word, char *new_word) {
    char buffer[200];
    char *ch;
 
    if (!(ch = strstr(str, old_word)))
        printf(str);
 
    strncpy(buffer, str, ch-str);
    buffer[ch-str] = 0;
    sprintf(buffer+(ch-str), "%s%s", new_word, ch+strlen(old_word));
    printf("%s\n", buffer);
}    

int main() {
    char str[] = "Hello, user!";
    printf("Old string: %s\n", str);
 
    search_replace(str, "user", "Programmer");
    return 0;
}
```

###
Вихід даного коду буде:

```
Old string: Hello, user!
Hello, Programmer!
```

## Поглиблений аналіз
**(1)** В історичному контексті, пошук та заміна в тексті був головною функцією текстових редакторів. Ці функції були і досі є незамінними у масовому перегляді й обробці даних.

**(2)** Є альтернативи такі як регулярні вирази, які надають більше гнучкості при пошуку паттернів тексту. 

**(3)** Щодо деталей реалізації, ми використовуємо функцію strstr() для знаходження першого входження старого слова в стрічці. Заміна відбувається шляхом копіювання частини стрічки до знайденого слова в буфер, а потім додавання нового слова й решти стрічки до буфера.

## Додатково
Ви можете дізнатись більше про текстові операції в C, відвідавши ці ресурси:
1. [Strings in C](https://www.geeksforgeeks.org/strings-in-c-2/)
2. [String Manipulations In C Programming Using Library Functions](https://www.softwaretestinghelp.com/c-strings-and-string-functions/)
3. [Regular Expressions in C](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)