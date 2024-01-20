---
title:                "Інтерполяція рядка"
html_title:           "Java: Інтерполяція рядка"
simple_title:         "Інтерполяція рядка"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Що це і навіщо?
Інтерполяція рядків - це вставка змінних або виразів прямо в текстовий рядок. Програмісти роблять це для динамічного створення тексту та вибіркового форматування.

## Як це робити:
В Haskell ми використовуємо бібліотеку `Text.Printf` для інтерполяції рядків. Посмотримо на приклад:

```Haskell
import Text.Printf

main = do
    let name = "Vasya"
    let age = 30
    putStrLn $ printf "My name is %s and I'm %d years old." name age
```

Виконавши цей код, ви отримаєте наступний вивід:

```Haskell
My name is Vasya and I'm 30 years old.
```

## Поглиблений занурення
1. **Історичний контекст:** Ідея інтерполяції рядків є достатньо старою і була вперше реалізована в мовах Perl та Unix shell. Haskell не має вбудованої підтримки для інтерполяції рядків, але надає можливість через бібліотеку `Text.Printf`.

2. **Альтернативи:** Програмісти можуть використовувати конкатенацію рядків або інльтерполяцію рядків з допомогою бібліотеки `Text.Printf`.

3. **Деталі реалізації:** `printf` використовується для форматування рядка, де `%s` і `%d` це заповнювачі, які замінюються аргументами, переданими printf.

## Дивіться також
1. [Printf для Haskell](http://hackage.haskell.org/package/base-4.12.0.0/docs/Text-Printf.html)
2. [Туторіал про правильне форматування рядків в Haskell](https://www.youtube.com/watch?v=H4CKZVGx3mw) 
3. [Неформальне введення в Haskell (Розділ про рядки)](http://learnyouahaskell.com/starting-out#strings)