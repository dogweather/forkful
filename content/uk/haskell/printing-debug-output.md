---
title:    "Haskell: Друк відладочного виведення"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Чому

Використання виведення налагодження (debug output) є корисним інструментом для розробників для відстеження проблем у своєму коді. Вона допомагає легше знайти та виправити помилки та оптимізувати роботу програми.

## Як

Для виведення налагодження в Хаскелі, використайте функцію `print` та рядок у подвійних лапках. Наприклад:

```Haskell
main = do
  print "Debug output приклад"
```

Цей код виведе `Debug output приклад` у консоль під час виконання програми. Ви також можете використовувати змінні та операції для виведення більш складної інформації.

```Haskell
main = do
  let x = 5
  let y = 7
  print ("Сума x та y дорівнює: " ++ show (x + y))
```

Цей код виведе `Сума x та y дорівнює: 12` у консоль.

## Глибокий розбір

Крім `print`, ще існує функція `trace` з модуля `Debug.Trace`, яка дозволяє вивести налагодження під час обчислення виразу. Наприклад:

```Haskell
import Debug.Trace

main = do
  let x = 5
  let y = 7
  let z = trace "x + y було обчислено" (x + y)
  print z
```

Цей код дозволить у виводі консолі побачити `x + y було обчислено` перед виведенням результату `12`. Це може бути корисно для відстеження проблем у складних функціях.

## Див назад

- [Haskell Debugging - Learn You a Haskell](https://learnyouahaskell.com/debugging)
- [Learn Haskell - Debugging](https://www.learnhaskell.org/debugging/)
- [Debugging Haskell in Visual Studio Code](https://medium.com/@dogwith1eye/debugging-haskell-in-visual-studio-code-ff8c6822f134)

## Дивіться також

- [Haskell для початківців - Принципи програмування](https://learnhaskell.dev/#/principles)
- [Haskell для початківців - Колекції та потоки даних](https://learnhaskell.dev/#/collections)
- [Вчимося від Mindset: Як стати кращим програмістом](https://learnhaskell.dev/#/mindset)