---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:39.063149-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0429\u043E\u0431 \u043F\u0440\u0430\u0446\u044E\u0432\u0430\u0442\u0438 \u0437\
  \ YAML \u0432 Elm, \u0432\u0430\u043C \u0437\u0430\u0437\u0432\u0438\u0447\u0430\
  \u0439 \u043F\u043E\u0442\u0440\u0456\u0431\u043D\u043E \u043F\u0435\u0440\u0435\
  \u0442\u0432\u043E\u0440\u0438\u0442\u0438 YAML \u043D\u0430 JSON \u043F\u043E\u0437\
  \u0430 Elm, \u0430 \u043F\u043E\u0442\u0456\u043C \u0432\u0438\u043A\u043E\u0440\
  \u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u0432\u0431\u0443\u0434\
  \u043E\u0432\u0430\u043D\u0456 \u0444\u0443\u043D\u043A\u0446\u0456\u0457 \u0434\
  \u0435\u043A\u043E\u0434\u0435\u0440\u0430\u2026"
lastmod: '2024-03-13T22:44:49.183543-06:00'
model: gpt-4-0125-preview
summary: "\u0429\u043E\u0431 \u043F\u0440\u0430\u0446\u044E\u0432\u0430\u0442\u0438\
  \ \u0437 YAML \u0432 Elm, \u0432\u0430\u043C \u0437\u0430\u0437\u0432\u0438\u0447\
  \u0430\u0439 \u043F\u043E\u0442\u0440\u0456\u0431\u043D\u043E \u043F\u0435\u0440\
  \u0435\u0442\u0432\u043E\u0440\u0438\u0442\u0438 YAML \u043D\u0430 JSON \u043F\u043E\
  \u0437\u0430 Elm, \u0430 \u043F\u043E\u0442\u0456\u043C \u0432\u0438\u043A\u043E\
  \u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u0432\u0431\u0443\
  \u0434\u043E\u0432\u0430\u043D\u0456 \u0444\u0443\u043D\u043A\u0446\u0456\u0457\
  \ \u0434\u0435\u043A\u043E\u0434\u0435\u0440\u0430 JSON Elm \u0434\u043B\u044F \u0440\
  \u043E\u0431\u043E\u0442\u0438 \u0437 \u0434\u0430\u043D\u0438\u043C\u0438."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 YAML"
weight: 41
---

## Як це зробити:
Щоб працювати з YAML в Elm, вам зазвичай потрібно перетворити YAML на JSON поза Elm, а потім використовувати вбудовані функції декодера JSON Elm для роботи з даними. Хоча цей підхід вимагає додаткового кроку конвертації, він використовує сильну систему типів Elm для забезпечення цілісності даних. Популярні інструменти для конвертації YAML в JSON включають онлайн-конвертери або сервіси на сервері. Після отримання JSON ви можете використовувати модуль `Json.Decode` Elm для роботи з даними.

Спочатку, припустимо, у вас є наступні дані YAML:

```yaml
person:
  name: Jane Doe
  age: 30
```

Перетворіть їх на формат JSON:

```json
{
  "person": {
    "name": "Jane Doe",
    "age": 30
  }
}
```

Потім визначте вашу модель Elm та декодер:

```elm
module Main exposing (..)

import Html exposing (text)
import Json.Decode as Decode

type alias Person =
    { name : String
    , age : Int
    }

personDecoder : Decode.Decoder Person
personDecoder =
    Decode.map2 Person
        (Decode.field "name" Decode.string)
        (Decode.field "age" Decode.int)

```

Щоб використати цей декодер для конвертації JSON у тип Elm:

```elm
import Json.Decode as Decode

jsonString = 
    """
    {
      "person": {
        "name": "Jane Doe",
        "age": 30
      }
    }
    """

decodeResult = Decode.decodeString (Decode.field "person" personDecoder) jsonString

main =
    case decodeResult of
        Ok person ->
            Html.text ("Привіт, " ++ person.name ++ "!")
            
        Err _ ->
            Html.text "Під час декодування сталася помилка."
```

Вивід (відображено в додатку Elm):
```
Привіт, Jane Doe!
```

Цей підхід забезпечує можливість працювати з даними YAML в Elm, використовуючи JSON як проміжний формат, і використовувати міцну систему типів та можливості декодування JSON Elm для безпечної та ефективної маніпуляції з зовнішніми даними.
