---
title:                "Haskell: Перетворення рядка на нижній регістр"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому

Перетворення рядка в нижній регістр є важливою функцією при роботі з текстовими даними в програмуванні. Вона дозволяє зробити дані більш узагальненими та універсальними для подальшого аналізу та обробки.

## Як виконати

Щоб перетворити рядок в нижній регістр в мові Haskell, використовуйте функцію `toLower` з модуля `Data.Char`. 
```Haskell
import Data.Char

lowerCaseString = map toLower "HELLO WORLD"

-- Output: "hello world"
```
Для виконання цієї функції, спочатку імпортуйте модуль `Data.Char`, щоб мати доступ до неї. Потім використовуйте функцію `map` для застосування функції `toLower` до кожного символу у рядку. Це дозволить знизити регістр кожного символу у рядку.

## Глибокий занурення

Перетворення рядка в нижній регістр може стати складнішою задачею при роботі з мультибайтовими символами, такими як кирилиця. Це через те, що вони можуть мати різний код символу для верхнього та нижнього регістру. Для обробки таких символів необхідно використовувати додаткові функції, наприклад, `toLower'` з модуля `Data.Text`.

## Дивіться також

- [Інструкція з перетворення рядка в нижній регістр в мові Haskell](https://www.geeksforgeeks.org/convert-string-to-lower-case-in-haskell/)
- [Виділення та маніпулювання текстовими даними у мові Haskell](https://www.haskell.org/tutorial/strings.html)
- [Документація по модулю `Data.Char` у мові Haskell](https://hackage.haskell.org/package/base/docs/Data-Char.html)