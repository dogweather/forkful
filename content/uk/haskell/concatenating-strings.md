---
title:                "Конкатенація рядків"
html_title:           "PHP: Конкатенація рядків"
simple_title:         "Конкатенація рядків"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо?

Складання рядків - це процес об'єднання двох або більше рядків в один. Це корисно коли програмісти потребують створити новий рядок з декількох існуючих.

## Як це робити:

Код на Haskell для складання рядків може бути таким:

```Haskell
main :: IO ()
main = do
    let a = "Привіт, "
    let b = "світ!"
    putStrLn (a ++ b) 
```

При виконанні цього коду ви отримаєте:

```Haskell
Привіт, світ!
```

## Занурення у деталі:

1) **Історичний контекст**: Конкатенація рядків - це фундаментальний аспект більшості мов програмування і Haskell не є винятком. 

2) **Альтернативи**: В Haskell есть також функція `concat`, яка об'єднує список рядків в один рядок.

```Haskell
main :: IO ()
main = do
    let listOfStrings = ["Привіт, ", "світ!"]
    putStrLn (concat listOfStrings)
```

Виходом буде той самий `Привіт, світ!`

3) **Деталі реалізації**: Операція складання рядків в Haskell реалізована через оператор `++`. Це непряма операция, оскільки для її виконання потрібно пройти по першому рядку, залишаючи вторий "незайнятим". 

## Див. також:

1) [Haskell documentation on Strings](https://www.haskell.org/tutorial/strings.html)
2) [StackOverflow: How do I concatenate strings?](https://stackoverflow.com/questions/1146824/how-do-i-concatenate-strings)