---
title:                "Разбор HTML"
date:                  2024-01-28T23:59:59.971257-07:00
model:                 gpt-4-0125-preview
simple_title:         "Разбор HTML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/gleam/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Разбор HTML означает преобразование строк HTML в структурированные данные. Программисты делают это, чтобы манипулировать или извлекать информацию из веб-страниц или безопасно генерировать HTML из пользовательского ввода.

## Как:
В Gleam нет встроенной библиотеки для разбора HTML, но вы можете использовать библиотеки Erlang через интероперабельность. Вот базовый пример использования пакета `meeseeks`, парсера HTML/XML:

Сначала добавьте `meeseeks` в зависимости вашего `rebar.config`, так:

```erlang
{deps, [
    {meeseeks, "0.15.0"}
]}.
```

Вот как вы можете разобрать и искать HTML в Gleam, предполагая, что вы правильно обработали взаимодействие с Erlang:

```gleam
import gleam/erlang
import meeseeks/html
import meeseeks/css

pub fn parse_and_find() -> Result(String, Nil) {
  let html = "<html><body><h1>Привет, Gleam!</h1></body></html>"
  let doc = html |> html.parse
  let selector = css.parse("h1").unwrap()
  
  doc
  |> meeseeks.all(selector)
  |> meeseeks.text
  |> Result.map(lists.head)
}
```
Эта функция анализирует HTML, затем запрашивает его на предмет тегов `h1` и получает текст. Вот что может вывести ее выполнение:

```shell
> parse_and_find()
Ok("Привет, Gleam!")
```

## Глубокое Погружение
Исторически, разбор HTML на новом языке означал написание собственного парсера или обёртывание существующего. Альтернативы включают использование регулярных выражений (обычно плохая идея из-за сложности HTML) или надёжные библиотеки, такие как `meeseeks`, основанные на проверенных парсерах (например, `html5ever` из Rust в случае `meeseeks`).

Реализация разбора HTML может быть сложной, потому что HTML часто бывает плохо сформирован или непредсказуем. Библиотеки справляются с этим, очищая и нормализуя данные. Взаимодействие с библиотеками Erlang из Gleam происходит легко благодаря совместимости экосистемы Erlang, что дает доступ к зрелым библиотекам без необходимости изобретать колесо заново.

## Смотрите также
Для дальнейшего чтения и ресурсов ознакомьтесь с:

- Библиотека Meeseeks на Hex: https://hex.pm/packages/meeseeks
- Парсер `html5ever` на Rust: https://github.com/servo/html5ever
- Руководство по взаимодействию с Erlang для Gleam: https://gleam.run/book/tour/erlang-interop/
