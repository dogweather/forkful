---
date: 2024-01-26 03:45:38.810394-07:00
description: "\u041E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u043D\u044F \u0447\u0438\
  \u0441\u0435\u043B \u043E\u0437\u043D\u0430\u0447\u0430\u0454 \u0457\u0445 \u043A\
  \u043E\u0440\u0438\u0433\u0443\u0432\u0430\u043D\u043D\u044F \u0434\u043E \u043D\
  \u0430\u0439\u0431\u043B\u0438\u0436\u0447\u043E\u0433\u043E \u0446\u0456\u043B\u043E\
  \u0433\u043E \u0430\u0431\u043E \u0437\u0430\u0437\u043D\u0430\u0447\u0435\u043D\
  \u043E\u0433\u043E \u0434\u0435\u0441\u044F\u0442\u043A\u043E\u0432\u043E\u0433\u043E\
  \ \u0440\u043E\u0437\u0440\u044F\u0434\u0443. \u041F\u0440\u043E\u0433\u0440\u0430\
  \u043C\u0456\u0441\u0442\u0438 \u043E\u043A\u0440\u0443\u0433\u043B\u044E\u044E\u0442\
  \u044C \u0447\u0438\u0441\u043B\u0430, \u0449\u043E\u0431 \u043A\u043E\u043D\u0442\
  \u0440\u043E\u043B\u044E\u0432\u0430\u0442\u0438\u2026"
lastmod: '2024-03-13T22:44:49.349588-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u043D\u044F \u0447\u0438\
  \u0441\u0435\u043B \u043E\u0437\u043D\u0430\u0447\u0430\u0454 \u0457\u0445 \u043A\
  \u043E\u0440\u0438\u0433\u0443\u0432\u0430\u043D\u043D\u044F \u0434\u043E \u043D\
  \u0430\u0439\u0431\u043B\u0438\u0436\u0447\u043E\u0433\u043E \u0446\u0456\u043B\u043E\
  \u0433\u043E \u0430\u0431\u043E \u0437\u0430\u0437\u043D\u0430\u0447\u0435\u043D\
  \u043E\u0433\u043E \u0434\u0435\u0441\u044F\u0442\u043A\u043E\u0432\u043E\u0433\u043E\
  \ \u0440\u043E\u0437\u0440\u044F\u0434\u0443. \u041F\u0440\u043E\u0433\u0440\u0430\
  \u043C\u0456\u0441\u0442\u0438 \u043E\u043A\u0440\u0443\u0433\u043B\u044E\u044E\u0442\
  \u044C \u0447\u0438\u0441\u043B\u0430, \u0449\u043E\u0431 \u043A\u043E\u043D\u0442\
  \u0440\u043E\u043B\u044E\u0432\u0430\u0442\u0438\u2026"
title: "\u041E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u043D\u044F \u0447\u0438\u0441\
  \u0435\u043B"
---

{{< edit_this_page >}}

## Що та чому?

Округлення чисел означає їх коригування до найближчого цілого або зазначеного десяткового розряду. Програмісти округлюють числа, щоб контролювати точність, адаптувати вивід для презентації користувачам або зменшити витрати обчислень для операцій з плаваючою комою.

## Як:

Haskell використовує функції `round`, `ceiling`, `floor` та `truncate` з `Prelude` для операций округлення.

```haskell
import Prelude

main :: IO ()
main = do
  let num = 3.567
  print $ round num    -- 4
  print $ ceiling num  -- 4
  print $ floor num    -- 3
  print $ truncate num -- 3
  
  -- Округлення до певного десяткового місця не входить до Prelude.
  -- Ось користувацька функція:
  let roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)
  print $ roundTo 1 num -- 3.6
```

## Поглиблений Огляд

Історично, округлення має значне значення в чисельному аналізі та комп'ютерних науках, оскільки воно критично важливе для мінімізації накопичення помилок у обчисленнях, особливо до стандартизації представлень плаваючою комою з IEEE 754.

До чого округляти? `round` веде до найближчого цілого—вгору чи вниз. `ceiling` та `floor` завжди округлюють вгору чи вниз до найближчого цілого відповідно, тоді як `truncate` просто відкидає десяткові знаки.

Альтернативи цим функціям можуть передбачати спеціальну логіку, як наш `roundTo`, або ви можете використовувати бібліотеки (наприклад, Data.Fixed) для більш складних вимог.

Уважно ставтеся до неочікуваних результатів через те, як Haskell обробляє півдорожні випадки в `round` (округлює до найближчого парного числа).

## Див. також

- Документація Haskell Prelude для функцій округлення: https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html
- Haskell Wiki про арифметику плаваючої коми: https://wiki.haskell.org/Floating_point_arithmetic
- Стандарт IEEE 754-2008 для більш детальної інформації про те, як обробляється плаваюча кома у багатьох мовах: https://ieeexplore.ieee.org/document/4610935
