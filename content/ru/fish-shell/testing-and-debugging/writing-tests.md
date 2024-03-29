---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:55.634009-07:00
description: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u0438\u0435 \u0442\u0435\u0441\
  \u0442\u043E\u0432 \u2014 \u044D\u0442\u043E \u0441\u043E\u0437\u0434\u0430\u043D\
  \u0438\u0435 \u043D\u0435\u0431\u043E\u043B\u044C\u0448\u0438\u0445 \u043F\u0440\
  \u043E\u0432\u0435\u0440\u043E\u043A \u0434\u043B\u044F \u0442\u043E\u0433\u043E\
  , \u0447\u0442\u043E\u0431\u044B \u0443\u0434\u043E\u0441\u0442\u043E\u0432\u0435\
  \u0440\u0438\u0442\u044C\u0441\u044F, \u0447\u0442\u043E \u0432\u0430\u0448 \u043A\
  \u043E\u0434 \u0440\u0430\u0431\u043E\u0442\u0430\u0435\u0442 \u0442\u0430\u043A\
  , \u043A\u0430\u043A \u043E\u0436\u0438\u0434\u0430\u0435\u0442\u0441\u044F. \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u043F\u0438\u0448\
  \u0443\u0442 \u0442\u0435\u0441\u0442\u044B,\u2026"
lastmod: '2024-03-13T22:44:45.846676-06:00'
model: gpt-4-0125-preview
summary: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u0438\u0435 \u0442\u0435\u0441\
  \u0442\u043E\u0432 \u2014 \u044D\u0442\u043E \u0441\u043E\u0437\u0434\u0430\u043D\
  \u0438\u0435 \u043D\u0435\u0431\u043E\u043B\u044C\u0448\u0438\u0445 \u043F\u0440\
  \u043E\u0432\u0435\u0440\u043E\u043A \u0434\u043B\u044F \u0442\u043E\u0433\u043E\
  , \u0447\u0442\u043E\u0431\u044B \u0443\u0434\u043E\u0441\u0442\u043E\u0432\u0435\
  \u0440\u0438\u0442\u044C\u0441\u044F, \u0447\u0442\u043E \u0432\u0430\u0448 \u043A\
  \u043E\u0434 \u0440\u0430\u0431\u043E\u0442\u0430\u0435\u0442 \u0442\u0430\u043A\
  , \u043A\u0430\u043A \u043E\u0436\u0438\u0434\u0430\u0435\u0442\u0441\u044F. \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u043F\u0438\u0448\
  \u0443\u0442 \u0442\u0435\u0441\u0442\u044B,\u2026"
title: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u0438\u0435 \u0442\u0435\u0441\u0442\
  \u043E\u0432"
---

{{< edit_this_page >}}

## Что и Зачем?
Написание тестов — это создание небольших проверок для того, чтобы удостовериться, что ваш код работает так, как ожидается. Программисты пишут тесты, чтобы заранее выявлять ошибки, экономить время и обеспечивать надёжность кода при его изменениях.

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
