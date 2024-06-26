---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:29.069806-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0427\u0442\u043E\u0431\u044B \u0440\u0430\u0431\u043E\u0442\u0430\
  \u0442\u044C \u0441 YAML \u0432 Haskell, \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u0443\u0439\u0442\u0435 \u043F\u0430\u043A\u0435\u0442 `yaml`. \u0421\u043D\u0430\
  \u0447\u0430\u043B\u0430 \u0443\u0441\u0442\u0430\u043D\u043E\u0432\u0438\u0442\u0435\
  \ \u0435\u0433\u043E."
lastmod: '2024-03-13T22:44:45.170495-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u043E\u0431\u044B \u0440\u0430\u0431\u043E\u0442\u0430\u0442\
  \u044C \u0441 YAML \u0432 Haskell, \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\
  \u0439\u0442\u0435 \u043F\u0430\u043A\u0435\u0442 `yaml`."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
weight: 41
---

## Как это сделать:
Чтобы работать с YAML в Haskell, используйте пакет `yaml`. Сначала установите его:

```shell
cabal install yaml
```

Определите структуру данных, а затем кодируйте и декодируйте YAML:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.YAML
import Data.ByteString (ByteString)

-- Определение структуры данных
data Person = Person
    { name :: String
    , age  :: Int
    } deriving (Show)

-- Пример экземпляра Person
examplePerson :: Person
examplePerson = Person "Chris" 30

-- Сериализация (Haskell в YAML)
yamlEncode :: Person -> ByteString
yamlEncode = encode

-- Десериализация (YAML в Haskell)
yamlDecode :: ByteString -> Either String Person
yamlDecode = decodeThrow

main :: IO ()
main = do
    -- Кодирование в YAML и вывод результата
    putStrLn "Encoded YAML:"
    print $ yamlEncode examplePerson
  
    -- Пример данных YAML
    let exampleYAML = "name: Alex\nage: 25\n"
  
    -- Декодирование из YAML и вывод результата
    putStrLn "Decoded Haskell:"
    print $ yamlDecode exampleYAML
```

Пример вывода для кодирования и декодирования:

```plaintext
Encoded YAML:
"age: 30\nname: Chris\n"
Decoded Haskell:
Right (Person {name = "Alex", age = 25})
```

## Глубокое погружение
YAML появился в 2001 году, нацеленный на сериализацию данных и читаемость для человека. Это популярный выбор для файлов конфигурации, таких как Docker Compose и рабочие процессы GitHub. К альтернативам относятся такие форматы, как JSON и XML, но минимальный синтаксис YAML часто предпочитают за его чистый вид. При реализации YAML в Haskell ключевым является определение структур данных, соответствующих парам ключ-значение в YAML. Пакет `yaml`, построенный на основе библиотеки C libyaml, обеспечивает отличную производительность и совместимость.

## Смотрите также
- Официальный сайт YAML: [https://yaml.org](https://yaml.org)
- Пакет `yaml` на Hackage: [https://hackage.haskell.org/package/yaml](https://hackage.haskell.org/package/yaml)
- Пакет `aeson`, для работы с JSON в Haskell, который имеет схожие черты: [https://hackage.haskell.org/package/aeson](https://hackage.haskell.org/package/aeson)
