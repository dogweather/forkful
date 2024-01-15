---
title:                "Капіталізація рядка"
html_title:           "Haskell: Капіталізація рядка"
simple_title:         "Капіталізація рядка"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому

Вони, можливо, хочуть використовувати вільний час для того, щоб познайомитися з новою мовою програмування, збагатити свої навички і стати більш кваліфікованими програмістами.

## Як це зробити

### Перший варіант: Використання функції `toUpper`

Найпростіший спосіб перетворити стрічку на капіталізовану є використання вбудованої функції `toUpper`, яка входить до складу модулю `Data.Char`. Для того, щоб користуватися цією функцією, необхідно імпортувати модуль у ваш файл програми, вказавши `import Data.Char` на початку файлу. Далі, застосовуючи функцію `toUpper` до будь-якого символу у вашій стрічці, після чого можна використовувати отримані дані для створення капіталізованої версії стрічки. 

```Haskell
import Data.Char

capitalizeString :: String -> String
capitalizeString str = map toUpper str
```

Приклад використання:

```Haskell
capitalizeString "hello world" -- "HELLO WORLD"
capitalizeString "haskell" -- "HASKELL"
capitalizeString "123abc" -- "123ABC"
```

### Другий варіант: Використання бібліотечної функції `capitalize`

Бібліотека `text` надає ще більше зручний спосіб для капіталізації стрічок - функція `capitalize`. Ця функція автоматично перший символ кожного слова у стрічці великою літерою, і залишає всі інші без змін.

```Haskell
import Data.Text (capitalize)

capitalizeString :: String -> String
capitalizeString str = capitalize (pack str)
```

Зверніть увагу, що використовуємо упаковану версію стрічки - `pack str` - оскільки функція `capitalize` приймає лише тип `Text`, а не `String`. 

Приклад використання:

```Haskell
capitalizeString "hello world" -- "Hello World"
capitalizeString "haskell" -- "Haskell"
capitalizeString "123abc" -- "123abc" (оскільки це не слово, перший символ не буде змінений)
```

## Глибокий дайв

Існує багато способів модифікувати стрічку у Haskell, і капіталізація - один з найпростіших прикладів. Як ви бачили з прикладів, є багато функцій, які можуть бути застосовані до стрічки для перетворення її у бажаний вигляд. Також, оскільки стрічки у Haskell є масивами символів, ви можете використовувати різноманітні функції, які опрацьовують масиви, для модифікації стріч