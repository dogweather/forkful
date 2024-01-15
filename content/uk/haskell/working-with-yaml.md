---
title:                "Робота з yaml."
html_title:           "Haskell: Робота з yaml."
simple_title:         "Робота з yaml."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Чому

Для чого комусь займатися роботою з YAML? Це текстовий формат, який дозволяє легко представляти структуровані дані та читати їх людиною.

## Як це робити

```Haskell
import qualified Data.Yaml as Yaml

-- Зчитуємо YAML файл
yamlFile <- Yaml.decodeFileThrow "example.yaml"

-- Виводимо дані з файлу
putStrLn $ show (yamlFile :: Yaml.Value)

-- Створюємо власний YAML об'єкт
let yamlObject = Yaml.object
                  [ ("key1", Yaml.string "value1")
                  , ("key2", Yaml.number 2)
                  ]

-- Записуємо об'єкт у файл
Yaml.encodeFile "output.yaml" yamlObject
```

Виведення:

```
Object (fromList [("key1",String "value1"),("key2",Number 2.0)])
```

## Глибоке занурення

У Haskell є багато бібліотек для роботи з YAML, але одна з найпопулярніших - це `Data.Yaml`. Вона надає можливість зчитувати та записувати дані з YAML формату за допомогою зручної функції `decodeFileThrow` та `encodeFile`. Також у бібліотеці є клас `FromJSON`, який дозволяє автоматично генерувати код для роботи з YAML, що зробить процес ще більш простим і зручним.

## Дивись також

- [Haskell бібліотека для роботи з YAML]()
- [Learning Haskell в 5 хвилин]()