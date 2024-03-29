---
date: 2024-01-26 00:53:30.452321-07:00
description: "\u041E\u0431\u0440\u043E\u0431\u043A\u0430 \u043F\u043E\u043C\u0438\u043B\
  \u043E\u043A \u0443 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0443\u0432\u0430\
  \u043D\u043D\u0456 \u0441\u0442\u043E\u0441\u0443\u0454\u0442\u044C\u0441\u044F\
  \ \u0443\u043F\u0440\u0430\u0432\u043B\u0456\u043D\u043D\u044F \u043D\u0435\u043F\
  \u0435\u0440\u0435\u0434\u0431\u0430\u0447\u0443\u0432\u0430\u043D\u0438\u043C \u2013\
  \ \u0441\u0438\u0442\u0443\u0430\u0446\u0456\u044F\u043C\u0438, \u043A\u043E\u043B\
  \u0438 \u0449\u043E\u0441\u044C \u043C\u043E\u0436\u0435 \u043F\u0456\u0442\u0438\
  \ \u043D\u0435 \u0442\u0430\u043A. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\
  \u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435 \u0434\
  \u043B\u044F \u0442\u043E\u0433\u043E, \u0449\u043E\u0431\u2026"
lastmod: '2024-03-13T22:44:49.372514-06:00'
model: gpt-4-1106-preview
summary: "\u041E\u0431\u0440\u043E\u0431\u043A\u0430 \u043F\u043E\u043C\u0438\u043B\
  \u043E\u043A \u0443 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0443\u0432\u0430\
  \u043D\u043D\u0456 \u0441\u0442\u043E\u0441\u0443\u0454\u0442\u044C\u0441\u044F\
  \ \u0443\u043F\u0440\u0430\u0432\u043B\u0456\u043D\u043D\u044F \u043D\u0435\u043F\
  \u0435\u0440\u0435\u0434\u0431\u0430\u0447\u0443\u0432\u0430\u043D\u0438\u043C \u2013\
  \ \u0441\u0438\u0442\u0443\u0430\u0446\u0456\u044F\u043C\u0438, \u043A\u043E\u043B\
  \u0438 \u0449\u043E\u0441\u044C \u043C\u043E\u0436\u0435 \u043F\u0456\u0442\u0438\
  \ \u043D\u0435 \u0442\u0430\u043A. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\
  \u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435 \u0434\
  \u043B\u044F \u0442\u043E\u0433\u043E, \u0449\u043E\u0431\u2026"
title: "\u041E\u0431\u0440\u043E\u0431\u043A\u0430 \u043F\u043E\u043C\u0438\u043B\u043E\
  \u043A"
---

{{< edit_this_page >}}

## Що та Чому?
Обробка помилок у програмуванні стосується управління непередбачуваним – ситуаціями, коли щось може піти не так. Програмісти роблять це для того, щоб забезпечити граціозну поведінку своїх програм у таких ситуаціях, без збоїв або виведення неправильних результатів.

## Як це робити:
Haskell надійно обробляє помилки за допомогою типів, таких як `Maybe` та `Either`. Ось швидкий погляд:

```Haskell
safeDivide :: Integral a => a -> a -> Maybe a
safeDivide _ 0 = Nothing  -- Ділення на нуль неприпустиме, тому повертаємо Nothing.
safeDivide x y = Just (x `div` y)  -- В іншому випадку все добре, повертаємо результат у Just.

-- Давайте подивимося на прикладі:
example1 :: Maybe Int
example1 = safeDivide 10 2  -- Just 5

example2 :: Maybe Int
example2 = safeDivide 10 0  -- Nothing
```

Для більш складної обробки помилок використовується `Either`:

```Haskell
safeDivideEither :: Integral a => a -> a -> Either String a
safeDivideEither _ 0 = Left "Помилка ділення на нуль."  -- Цього разу помилка містить повідомлення.
safeDivideEither x y = Right (x `div` y)

-- І на прикладі:
example3 :: Either String Int
example3 = safeDivideEither 10 2  -- Right 5

example4 :: Either String Int
example4 = safeDivideEither 10 0  -- Left "Помилка ділення на нуль."
```

## Поглиблений огляд
У світі Haskell обробка помилок має довгу історію. Колись помилки могли повалити вашу цілу програму – анітрохи не весело. Типова система Haskell пропонує способи, щоб зробити це набагато менш ймовірним. У нас є `Maybe` та `Either`, але також існують інші, такі як `Exceptions` та `IO` для різних сценаріїв.

`Maybe` простий: ви отримуєте `Just` щось, якщо все гаразд, або `Nothing`, якщо ні. `Either` підвищує латку, дозволяючи вам повертати повідомлення про помилку (`Left`) або успішний результат (`Right`).

Обидва є чистими, це означає, що вони не взаємодіють з зовнішнім світом – справа велика в Haskell. Ми уникаємо пасток неконтрольованих винятків, які турбують деякі інші мови.

Для тих, хто не задовольняється `Maybe` та `Either`, бібліотеки, такі як `Control.Exception`, надають більш традиційне, імперативне оброблення помилок через винятки. Але їхнє надмірне використання може ускладнити речі, тому спільнота частіше залишається при типах.

## Дивіться також
Поглиблюйте знання з:

- власні документи Haskell: [Haskell](https://haskell.org/documentation)
- Відмінно для початківців: ["Вивчай Haskell задля великого і доброго!"](http://learnyouahaskell.com/)
