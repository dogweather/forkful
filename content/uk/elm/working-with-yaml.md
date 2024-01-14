---
title:                "Elm: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Чому

Що саме робить YAML таким популярним серед розробників? Навіщо взагалі займатися програмуванням з використанням YAML? В цьому блозі ми розглянемо причини, чому YAML є потужним інструментом для розробки, і як він може полегшити вам життя.

## Як

Якщо ви розумієте чому YAML використовується, то перейдемо до того, як з ним працювати. На щастя, Elm має дуже зручний пакет для роботи з YAML - elm-yaml. Давайте подивимось на приклади коду, які допоможуть нам краще зрозуміти його застосування.

```Elm

import Yaml

-- Створюємо YAML рядок

let yamlString = Yaml.encode (
    Yaml.scalar "title" "Привет, Україно!"
)

-- Виводимо результат

Yaml.toString yamlString
```

```Elm
import Yaml

-- Створимо список з YAML об'єктів

let yamlList = Yaml.encodeList [
    Yaml.object [
        ("name", Yaml.scalar "John"),
        ("age", Yaml.scalar "25")
    ],
    Yaml.object [
        ("name", Yaml.scalar "Jane"),
        ("age", Yaml.scalar "30")
    ]
]

-- Виводимо результат

Yaml.toString yamlList
```

## Глибше копаємо

Тепер, коли ми розуміємо основи, давайте подивимось на деякі з приємних особливостей YAML.

### Перехресні посилання

Одна з найкрутіших рис YAML - це можливість створювати перехресні посилання між різними об'єктами та списками. Для цього використовується спеціальна скалярна значення `*ref` та `@ref`.

```Elm

let yamlObject = Yaml.object [
    ("name", Yaml.scalar "John"),
    ("bestFriend", Yaml.scalar "*ref"),
    ("hobbies", Yaml.list [
        Yaml.scalar "sports",
        Yaml.scalar "music",
        Yaml.scalar "@ref"
    ]),
    ("ref", Yaml.scalar "music")
]

-- Результат

{
    "name": "John",
    "bestFriend": *ref,
    "hobbies": [
        "sports",
        "music",
        @ref
    ],
    "ref": "music"
}
```

### Довільні ключі та значення

В YAML також можна використовувати будь-які ключі та значення, що дозволяє створювати більш гнучкі та складні структури даних.

```Elm

let yamlObject = Yaml.object [
    ("1", Yaml.scalar "one"),
    ("2", Yaml.scalar "two"),
    ("3", Yaml.scalar "three"),
    ("4", Yaml.scalar "four")
]

-- Результат

{
    "1": "one",
    "2": "two",
    "3": "three",
    "4": "four"
}
```

## Дивись також

- [Офіційна документація по YAML](https://yaml.org/)
- [Пакет Elm для роботи з YAML](https://package.elm-lang.org/packages/avh4/elm-yaml/latest)