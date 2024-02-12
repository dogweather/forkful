---
title:                "Обробка помилок"
aliases:
- /uk/haskell/handling-errors.md
date:                  2024-01-26T00:53:30.452321-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обробка помилок"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/handling-errors.md"
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
