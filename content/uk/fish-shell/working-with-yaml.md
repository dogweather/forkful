---
title:                "Робота з yaml"
html_title:           "Fish Shell: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Що і Чому?

Робота з YAML - це популярний спосіб для програмістів організувати та зберігати дані в структурованому форматі, який легко підходить для людей та машин. Використовуючи YAML, ви можете створювати файли конфігурації та іншу структуровану інформацію для вашого програмного забезпечення.

## Як:

Код-приклади та вихідні дані ми покажемо в цьому розділі за допомогою блоків коду ```Fish Shell ... ```. Для початку, додайте розширення ```.yml``` до вашого файлу YAML, щоб Fish Shell розпізнав його як файл, що містить дані у цьому форматі.

#### Читання даних з файлу YAML

```
Fish Shell >>> set data (yq e '.key' path/to/file.yml)
```

#### Запис даних в файл YAML

```
Fish Shell >>> set key "value"
Fish Shell >>> echo $key > path/to/file.yml
```

#### Парсінг даних у форматі YAML

``` 
Fish Shell >>> set data "key: value"
Fish Shell >>> echo $data | yq r - key
```

## Глибше

YAML був створений у 2001 році як спосіб для людей записувати та читати дані нарізані у структурованому форматі. Це популярний замінник XML та JSON, оскільки має простий та читабельний синтаксис. Існують багато бібліотек та інструментів для роботи з YAML у різних мовах програмування, включаючи Python, Ruby та Java.

## Дивись також

- [YAML Official Website](https://yaml.org/)
- [YAML Spec](https://yaml.org/spec/)
- [YAML Tutorial](https://rollout.io/blog/yaml-tutorial-everything-you-need-get-started/)