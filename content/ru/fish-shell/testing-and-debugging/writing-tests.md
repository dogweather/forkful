---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:55.634009-07:00
description: "\u041A\u0430\u043A: \u0412 Shell Fish \u043D\u0435\u0442 \u0432\u0441\
  \u0442\u0440\u043E\u0435\u043D\u043D\u043E\u0439 \u0442\u0435\u0441\u0442\u043E\u0432\
  \u043E\u0439 \u0441\u0438\u0441\u0442\u0435\u043C\u044B, \u043D\u043E \u0432\u044B\
  \ \u043C\u043E\u0436\u0435\u0442\u0435 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u043E\u0432\u0430\u0442\u044C `fisher` \u0434\u043B\u044F \u0443\u0441\u0442\u0430\
  \u043D\u043E\u0432\u043A\u0438 \u0442\u0430\u043A\u043E\u0439 \u0441\u0438\u0441\
  \u0442\u0435\u043C\u044B, \u043A\u0430\u043A `Fishtape`. \u0412\u043E\u0442 \u043F\
  \u0440\u043E\u0441\u0442\u043E\u0439 \u0442\u0435\u0441\u0442 \u0441\u2026"
lastmod: '2024-03-13T22:44:45.846676-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Shell Fish \u043D\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\u0435\
  \u043D\u043D\u043E\u0439 \u0442\u0435\u0441\u0442\u043E\u0432\u043E\u0439 \u0441\
  \u0438\u0441\u0442\u0435\u043C\u044B, \u043D\u043E \u0432\u044B \u043C\u043E\u0436\
  \u0435\u0442\u0435 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\
  \u044C `fisher` \u0434\u043B\u044F \u0443\u0441\u0442\u0430\u043D\u043E\u0432\u043A\
  \u0438 \u0442\u0430\u043A\u043E\u0439 \u0441\u0438\u0441\u0442\u0435\u043C\u044B\
  , \u043A\u0430\u043A `Fishtape`."
title: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u0438\u0435 \u0442\u0435\u0441\u0442\
  \u043E\u0432"
weight: 36
---

## Как:
В Shell Fish нет встроенной тестовой системы, но вы можете использовать `fisher` для установки такой системы, как `Fishtape`. Вот простой тест с `Fishtape`:

```fish
# Сначала установите Fishtape
fisher install jorgebucaran/fishtape

# Создайте тестовый файл, `test_my_function.fish`
function test_my_function
    echo "Запуск тестов my_function"

    # Тестовый случай
    my_function argument
    echo $status | fishtape
end

# Запустите ваш тестовый файл в Fish Shell
fishtape test_my_function.fish
```

Пример вывода может выглядеть так:

```
TAP version 13
ok 1 my_function с аргументом

1..1
# тесты 1
# прошло  1

# ок
```

## Погружение
Fish shell появился в 2005 году, значительно позже Bash. С самого начала он был ориентирован на умные функции и удобство для пользователя. В отличие от Bash, он изначально не содержит множество тестовых инструментов. Здесь на помощь приходят сторонние инструменты, такие как `Fishtape`, добавляя отсутствующую функциональность тестирования в Fish. Помните, скрипты Fish могут быть протестированы как любой другой скрипт — путём проверки вывода и статусов выхода, но с `Fishtape` вы получаете вывод, соответствующий TAP, который легче использовать в CI/CD пайплайнах и с испытательными стендами.

## Смотрите также
Ознакомьтесь с этими ресурсами, чтобы углубиться в тему Fish Shell и `Fishtape`:
- [Официальная документация Fish](https://fishshell.com/docs/current/index.html)
- [Fishtape на GitHub](https://github.com/jorgebucaran/fishtape)
- [Менеджер плагинов Fisher](https://github.com/jorgebucaran/fisher)
