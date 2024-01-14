---
title:    "C: Перетворення рядка в нижній регістр"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому

Різні завдання можуть вимагати перетворення рядка на нижній регістр. Це може бути необхідно для порівняння рядків або для забезпечення єднакового форматування при відображенні тексту.

## Як це зробити

```C 
#include <stdio.h>
#include <string.h>
int main()
{
    char string[] = "Hello, World!";
    // перетворюємо рядок на нижній регістр
    for (int i = 0; i < strlen(string); i++) {
        string[i] = tolower(string[i]);
    }
    // виводимо результат
    printf("%s", string);
    return 0;
}
```
Вивід: hello, world!

## Profound Dive

Функція tolower () з бібліотеки <string.h> може бути використана для перетворення символів з великими літерами на символи з маленькими літерами. Важливо пам'ятати, що ця функція працює тільки з кодуванням ASCII, тому можуть виникнути проблеми з перетворенням рядків, що містять символи інших кодувань.

## Дивіться також 

[Офіційна документація стандартної бібліотеки С для функції tolower()](https://www.cplusplus.com/reference/cctype/tolower/) 

[Стаття на Medium про перетворення рядка на нижній регістр в С](https://medium.com/@kimiyukiyukawa/writing-a-letter-character-to-all-lower-case-in-c-programming-language-33d1fc2a3f6b)

[Демонстрація використання функції tolower() для перетворення рядка на нижній регістр](https://www.geeksforgeeks.org/convert-string-lower-case-using-c/)