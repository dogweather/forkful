---
title:                "Використання регулярних виразів"
html_title:           "Haskell: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Чому

Найочікуваніша вагома причина використання регулярних виразів - це їх універсальність та зручність в обробці текстових даних.

## Як використовувати

Для використання регулярних виразів в Haskell потрібно додати модуль `Text.Regex.Posix` та використовувати функцію `=~` для зіставлення з текстовим виразом. Ось приклад, який знаходить у тексті всі слова, що закінчуються на "ку":
```Haskell
import Text.Regex.Posix

main = do
    let text = "Марійка любить піти в кіно"
        pattern = "ку\\b"
    print $ text =~ pattern -- виведе "ку"
```

## Глибока навчання

Використання функції `=~` дозволяє застосовувати різні вирази для знаходження та зміни текстових даних. Також, модуль `Text.Regex.Posix` містить багато корисних функцій для роботи з патернами. Детальну інформацію про всі можливості цього модуля можна знайти в [офіційній документації](https://hackage.haskell.org/package/regex-posix/docs/Text-Regex-Posix.html).

## Дивись також

- [Офіційна документація](https://hackage.haskell.org/package/regex-posix/docs/Text-Regex-Posix.html)
- [Огляд регулярних виразів в Haskell](https://wiki.haskell.org/Regular_expressions)