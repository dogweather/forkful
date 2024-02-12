---
title:                "Написание тестов"
aliases: - /ru/fish-shell/writing-tests.md
date:                  2024-01-29T00:05:55.634009-07:00
model:                 gpt-4-0125-preview
simple_title:         "Написание тестов"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/fish-shell/writing-tests.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
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
