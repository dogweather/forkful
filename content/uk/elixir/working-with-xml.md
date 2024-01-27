---
title:                "Робота з XML"
date:                  2024-01-26T04:30:15.782157-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з XML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/working-with-xml.md"
---

{{< edit_this_page >}}

## Що і чому?
Робота з XML в Elixir означає розбір, створення та маніпуляції з даними XML. Програмісти звертаються до XML, оскільки він широко поширений у веб-сервісах, конфігураційних файлах та системах зі спадщиною.

## Як користуватися:
Elixir стандартно не включає розбір XML до своєї бібліотеки. SweetXML є популярним вибором. Ось як його використовувати:

```elixir
# Додайте SweetXML до вашого списку залежностей у mix.exs
{:sweet_xml, "~> 0.6"}

# У вашому коді
import SweetXml

xml = """
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Reminder</heading>
  <body>Не забудь мене цими вихідними!</body>
</note>
"""

# Розбір XML
note = xml |> xpath(~x"//note")
to = xml |> xpath(~x"//note/to" |> inner_text())
IO.puts to # Вивід: Tove
```

## Поглиблений розгляд
XML, або мова розмітки з можливістю розширення, існує з кінця 90-х. Він марнотратний, але структурований — ідеальний для обміну складними даними. Незважаючи на те, що популярність JSON стрімко зросла через його простоту, XML залишається закріпленим у багатьох підприємницьких та фінансових системах через свою виразність та стандартизовані схеми.

Альтернативи включають:
- JSON для більш легкого, менш марнотратного обміну даними.
- Protobuf або Thrift для бінарної серіалізації даних, зокрема для внутрішніх систем.

Під капотом, бібліотеки XML для Elixir використовують бібліотеку :xmerl Erlang для розбору, яка надає надійну підтримку, але може бути менш інтуїтивною, ніж більш сучасні підходи. Оскільки Elixir розвивається, бібліотеки, керовані спільнотою, такі як SweetXML, огортають ці можливості більш еліксірною синтаксичною конструкцією, роблячи маніпуляції з XML доступнішими.

## Дивіться також:
- SweetXML на Hex: https://hex.pm/packages/sweet_xml
- Погляд Elixir на розбір XML: https://elixir-lang.org/getting-started/mix-otp/dependencies-and-umbrella-projects.html
- Документація xmerl для базової роботи з XML: http://erlang.org/doc/apps/xmerl/index.html