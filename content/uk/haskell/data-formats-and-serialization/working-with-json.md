---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:12.343219-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON (JavaScript Object\
  \ Notation) \u0432 Haskell \u043F\u043E\u043B\u044F\u0433\u0430\u0454 \u0443 \u0440\
  \u043E\u0437\u0431\u043E\u0440\u0456 \u0434\u0430\u043D\u0438\u0445 JSON \u0434\u043E\
  \ \u0442\u0438\u043F\u0456\u0432 Haskell \u0442\u0430 \u043A\u043E\u043D\u0432\u0435\
  \u0440\u0442\u0430\u0446\u0456\u0457 \u0442\u0438\u043F\u0456\u0432 Haskell \u043D\
  \u0430\u0437\u0430\u0434 \u0443 JSON. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u0456\u0441\u0442\u0438\u2026"
lastmod: '2024-02-25T18:49:46.875483-07:00'
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON (JavaScript Object Notation)\
  \ \u0432 Haskell \u043F\u043E\u043B\u044F\u0433\u0430\u0454 \u0443 \u0440\u043E\u0437\
  \u0431\u043E\u0440\u0456 \u0434\u0430\u043D\u0438\u0445 JSON \u0434\u043E \u0442\
  \u0438\u043F\u0456\u0432 Haskell \u0442\u0430 \u043A\u043E\u043D\u0432\u0435\u0440\
  \u0442\u0430\u0446\u0456\u0457 \u0442\u0438\u043F\u0456\u0432 Haskell \u043D\u0430\
  \u0437\u0430\u0434 \u0443 JSON. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\
  \u0441\u0442\u0438\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON"
---

{{< edit_this_page >}}

## Що і чому?
Робота з JSON (JavaScript Object Notation) в Haskell полягає у розборі даних JSON до типів Haskell та конвертації типів Haskell назад у JSON. Програмісти роблять це для того, щоб забезпечити безперебійний обмін даними між їх Haskell додатками та веб-сервісами або API, що є загальним правилом у сучасній розробці програмного забезпечення для міжплатформного обміну даними.

## Як це зробити:
Haskell не має вбудованої підтримки JSON, як JavaScript, але з допомогою сторонніх бібліотек, таких як **Aeson**, робота з JSON стає простою. Aeson надає як високорівневі, так і низькорівневі функції для кодування (перетворення значень Haskell у JSON) та декодування (розбір JSON у значення Haskell).

### Встановлення Aeson
Спочатку додайте Aeson до залежностей вашого проекту, оновивши ваш файл `.cabal` або використавши Stack або Cabal безпосередньо:

```shell
cabal update && cabal install aeson
```
або, якщо ви використовуєте Stack:
```shell
stack install aeson
```

### Розбір JSON
Почнемо з основного прикладу декодування даних JSON до типу Haskell. Припустімо, у нас є наступний JSON, що представляє особу:

```json
{
  "name": "John Doe",
  "age": 30
}
```

Спочатку визначте відповідний тип даних Haskell та зробіть його екземпляром `FromJSON`:

```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as B

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Generic, Show)

instance FromJSON Person

-- Функція для декодування JSON з файлу
decodePerson :: FilePath -> IO (Maybe Person)
decodePerson filePath = do
  personJson <- B.readFile filePath
  return $ decode personJson
```
Використання:
Припускаючи, що `person.json` містить вищезазначені дані JSON, запустіть:
```haskell
main :: IO ()
main = do
  maybePerson <- decodePerson "person.json"
  print maybePerson
```
Приклад виводу:
```haskell
Just (Person {name = "John Doe", age = 30})
```

### Кодування значень Haskell як JSON
Для конвертації значення Haskell назад у JSON, вам потрібно зробити ваш тип екземпляром `ToJSON`, а потім використати `encode`.

```haskell
import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)

-- Припускаючи тип Person з попереднього прикладу

instance ToJSON Person

encodePerson :: Person -> B.ByteString
encodePerson = encode

main :: IO ()
main = do
  let person = Person "Jane Doe" 32
  putStrLn $ show $ encodePerson person
```
Приклад виводу:
```json
{"name":"Jane Doe","age":32}
```

Ці приклади демонструють основи роботи з JSON в Haskell за допомогою Aeson. Пам'ятайте, що Aeson пропонує набагато більше, включаючи користувацькі правила розбору, роботу з складними вкладеними JSON та багато іншого, що підходить для різних потреб і сценаріїв.
