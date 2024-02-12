---
title:                "Чтение аргументов командной строки"
date:                  2024-01-29T00:00:53.185879-07:00
model:                 gpt-4-0125-preview
simple_title:         "Чтение аргументов командной строки"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elixir/reading-command-line-arguments.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?
Чтение аргументов командной строки позволяет программе получать данные напрямую из терминала — такие как настройки или имена файлов. Программисты делают это для настройки поведения программы без изменения кода.

## Как:

В Elixir, получение аргументов командной строки — проще простого. Используйте `System.argv()`, и вы получите их как список строк.

```elixir
defmodule CliArgs do
  def main do
    args = System.argv()
    IO.inspect(args)
  end
end

CliArgs.main()
```

Запустите это как `elixir cli_args.exs foo bar baz`, и ожидайте:

```
["foo", "bar", "baz"]
```

Вы видите аргументы `foo`, `bar` и `baz` прямо там, в списке Elixir.

## Глубокое погружение

Исторически, аргументы командной строки существуют очень давно, происходя из ранних сред CLI. В Elixir, `System.argv()` — это ваш верный конь для этой работы. Почему? Потому что он встроен в Erlang VM, на котором построен Elixir. 

Альтернативы? Конечно, есть библиотеки, которые разбирают аргументы за вас, добавляя флаги и опции. Но для базового Elixir, `System.argv()` — это путь к следованию.

С точки зрения реализации, важно помнить, что `System.argv()` дает вам все аргументы в виде строк. Если вам нужны числа или другие типы, вам придется конвертировать их вручную. Также, порядок имеет значение. Ваш первый аргумент командной строки — это `List.first(System.argv())`, и так далее.

## Смотрите также

Для большей информации, смотрите:
- [Документация модуля System в Elixir](https://hexdocs.pm/elixir/System.html) для других полезных системных функций.
- [Optparse](https://hexdocs.pm/elixir/OptionParser.html) в стандартной библиотеке Elixir, который является зверем для разбора опций командной строки.
- [Документация init в Erlang](http://erlang.org/doc/man/init.html), если вам интересно заглянуть под капот VM, который поддерживает Elixir.