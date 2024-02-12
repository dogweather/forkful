---
title:                "Визначення довжини рядка"
aliases:
- uk/haskell/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:34.706774-07:00
model:                 gpt-4-1106-preview
simple_title:         "Визначення довжини рядка"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Що це та навіщо?

Обчислення довжини рядка - це спосіб визначити, скільки символів містить окремий рядок. Програмісти роблять це для валідації вводу, здійснення обмежень на розмір тексту, та підрахунку даних для подальших обчислень.

## Як це зробити:

```Haskell
main :: IO ()
main = do
    let str = "Привіт, Haskell!"
    print $ length str
```

Вивід:
```
16
```

## Заглиблення:

Сама функція `length` в Haskell насправді проста, але вона не обмежується рядками ─ вона приймає будь-який список. Оскільки рядок є списком символів, `length` просто підраховує елементи цього списку.

Історично, функція `length` є частиною стандартної бібліотеки Haskell з самого початку. Але слід зауважити, що вона не завжди найкращий вибір для великих текстових даних через свою обчислювальну складність у O(n).

Щодо альтернатив, для більших або лінивих структур даних існує `Data.Text.length` з бібліотеки `text`, яка оптимізована для роботи із текстовими даними.

```Haskell
import qualified Data.Text as T

main :: IO ()
main = do
    let str = T.pack "Привіт, Haskell!"
    print $ T.length str
```

Така оптимізація бібліотеки `text` дозволяє більш ефективно працювати з великими чи потоковими даними, оскільки `Data.Text` представляє текст у компактному і ефективному у пам'яті форматі.

## Послання на джерела:

- Офіційна документація Haskell про String і функції `length`: https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-List.html#v:length
- Бібліотека `text` для оптимізації текстових операцій: https://hackage.haskell.org/package/text
- Увод у стандартні типи даних Haskell, включно з `String`: https://www.haskell.org/tutorial/stdtypes.html
