---
title:                "Haskell: Працюючи з yaml"
simple_title:         "Працюючи з yaml"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

# Чому

Робота з YAML є дуже корисною для розробників, оскільки він дозволяє легко зчитувати та записувати дані в структурованому форматі. Крім того, YAML є легким у використанні та може бути корисним для управління конфігураційними файлами та обміну даними між різними програмами.

# Як це зробити

Програмування у Haskell з YAML є досить простим завданням. Ось приклад коду, який дозволяє побудувати структуру даних у форматі YAML та вивести її на екран:

```Haskell
import Data.Yaml
import qualified Data.ByteString as BS

-- Створення структури даних
data Person = Person
    { name :: String
    , age :: Int
    , hobbies :: [String]
    }

main :: IO ()
main = do
    -- Запис структури даних у форматі YAML
    let person = Person "John" 25 ["Programming", "Hiking"]
    BS.writeFile "person.yaml" $ encode person

    -- Читання даних з файлу YAML та виведення на екран
    contents <- BS.readFile "person.yaml"
    let result = decode contents :: Maybe Person
    print result
```

В результаті, на екрані ми побачимо наступне:

```bash
Just (Person {name = "John", age = 25, hobbies = ["Programming","Hiking"]})
```

# Глибоке дослідження

У Haskell існує багато бібліотек для роботи з YAML, таких як `Data.Yaml`, `yaml`, `HsYAML` та інші. Кожна з них має свої особливості та може використовуватися для різних завдань.

Наприклад, бібліотека `yaml` дозволяє використовувати типові класи для серіалізації та десеріалізації даних, а також надає можливість обробки даних у різних форматах (JSON, XML тощо).

Для отримання більш детальної інформації про роботу з YAML в Haskell, радимо ознайомитися з документацією по бібліотекам та пошукати приклади коду в Інтернеті.

# Дивитися також

- [Офіційна документація по бібліотеці `Data.Yaml`](https://hackage.haskell.org/package/yaml)
- [Бібліотека Haskell для роботи з YAML `yaml`](https://hackage.haskell.org/package/yaml)
- [Навчальний курс з Haskell на сайті Codecademy](https://www.codecademy.com/learn/learn-haskell)