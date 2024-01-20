---
title:                "Зробити першу літеру рядка великою"
html_title:           "Haskell: Зробити першу літеру рядка великою"
simple_title:         "Зробити першу літеру рядка великою"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що та навіщо?
Перетворення на великі літери інакше відоме як капіталізація рядків - це процес заміни першої літери слова на його велику форму. Програмісти роблять це для поліпшення читабельності або для задоволення вимог стилю коду.

## Як це робиться:
В Haskell для того, щоб перетворити рядок на рядок з великої літери, використовується функція `toUpper` з модуля `Data.Char`. Зразок наведено нижче:

```Haskell
import Data.Char

capitalize :: String -> String
capitalize (c:cs) = toUpper c : cs
```

Якщо ми подамо функції "capitalize", рядок "hello": `capitalize "hello"` - отримаємо рядок "Hello".

## В глибинах:

1. Історичний контекст: Перетворення рядків на рядок з великої літери відбувається у багатьох мовах програмування. Це не унікальна особливість Haskell, а загальний принцип, що існує в програмуванні взагалі.

2. Альтернативи: Ви також можете використовувати функцію `map` для застосування `toUpper` до кожного символу у рядку, якщо потрібно робити весь рядок рядком з великих літер.

3. Деталі реалізації: Хотя ми зосереджуємось на рядках ASCII, функція `toUpper` також може працювати з символами Unicode. Профайлер Haskell подбає про цей аспект.

## Диви також:
1. Data.Char: https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html
2. Загальний посібник по Haskell: http://learnyouahaskell.com/chapters
3. Робота з рядками в Haskell: https://www.tutorialspoint.com/haskell/haskell_strings.htm