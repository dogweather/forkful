---
title:                "Работа с XML"
date:                  2024-01-29T00:04:57.428577-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elixir/working-with-xml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Работа с XML в Elixir подразумевает разбор, создание и манипулирование данными XML. Программисты обращаются к XML, поскольку он широко распространен в веб-сервисах, конфигурационных файлах и устаревших системах.

## Как это сделать:
Elixir не включает разбор XML в свою стандартную библиотеку. Популярным выбором является SweetXML. Вот как его использовать:

```elixir
# Добавьте SweetXML в ваши зависимости в mix.exs
{:sweet_xml, "~> 0.6"}

# В вашем коде
import SweetXml

xml = """
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Reminder</heading>
  <body>Не забудьте про меня на этом выходных!</body>
</note>
"""

# Разбор XML
note = xml |> xpath(~x"//note")
to = xml |> xpath(~x"//note/to" |> inner_text())
IO.puts to # Вывод: Tove
```

## Подробнее
XML или Расширяемый язык разметки существует с конца 90-х. Он многословен, но структурирован — идеален для обмена сложными данными. Хотя популярность JSON выросла благодаря его простоте, XML остается закрепленным во многих предприятиях и финансовых системах благодаря своей выразительности и стандартизированным схемам.

Альтернативы включают:
- JSON для более легкого, менее многословного обмена данными.
- Protobuf или Thrift для бинаризованной сериализованной коммуникации данных, особенно для внутренних систем.

Внутри, библиотеки XML для Elixir используют библиотеку :xmerl Erlang для разбора, которая обеспечивает надежную поддержку, но может быть менее интуитивно понятной, чем более современные подходы. По мере развития Elixir, сообществом разработанные библиотеки, такие как SweetXML, оборачивают эти с Elixir-ориентированным синтаксисом, делая манипуляции с XML более доступными.

## Смотрите также:
- SweetXML на Hex: https://hex.pm/packages/sweet_xml
- Мнение Elixir о разборе XML: https://elixir-lang.org/getting-started/mix-otp/dependencies-and-umbrella-projects.html
- Документация xmerl для базовой обработки XML: http://erlang.org/doc/apps/xmerl/index.html
