---
title:                "Аналіз HTML"
aliases:
- uk/elixir/parsing-html.md
date:                  2024-02-03T19:12:13.193706-07:00
model:                 gpt-4-0125-preview
simple_title:         "Аналіз HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і Чому?

Парсинг HTML у мові програмування Elixir полягає у витягуванні інформації з HTML-документів. Програмісти роблять це для програмної взаємодії з веб-сторінками, збору даних або автоматизації веб-взаємодій, дозволяючи додаткам динамічно розуміти та використовувати веб-контент.

## Як це зробити:

Elixir, з його міцною моделлю конкурентності та парадигмою функціонального програмування, не має вбудованих можливостей для парсингу HTML. Втім, ви можете використовувати популярні сторонні бібліотеки, такі як `Floki`, для цієї мети. Floki робить парсинг HTML інтуїтивно зрозумілим та ефективним, використовуючи можливості зіставлення зразків та пайпінгу Elixir.

Спочатку, додайте Floki до залежностей вашого mix.exs:

```elixir
defp deps do
  [
    {:floki, "~> 0.31.0"}
  ]
end
```

Потім, запустіть `mix deps.get`, щоб встановити нову залежність.

Тепер, давайте проаналізуємо простий HTML-рядок, щоб витягнути дані. Ми шукатимемо заголовки всередині тегів `<h1>`:

```elixir
html_content = """
<html>
  <body>
    <h1>Привіт, Elixir!</h1>
    <h1>Ще один заголовок</h1>
  </body>
</html>
"""

titles = html_content
         |> Floki.find("h1")
         |> Floki.text()

IO.inspect(titles)
```

**Приклад виводу:**

```elixir
["Привіт, Elixir!", "Ще один заголовок"]
```

Щоб заглибитись більше, скажімо, ви хочете витягнути посилання (теги `<a>`) разом з їхніми атрибутами href. Ось як ви можете це зробити:

```elixir
html_content = """
<html>
  <body>
    <a href="https://elixir-lang.org/">Офіційний вебсайт Elixir</a>
    <a href="https://hexdocs.pm/">HexDocs</a>
  </body>
</html>
"""

links = html_content
        |> Floki.find("a")
        |> Enum.map(fn({_, attrs, [text]}) -> {text, List.keyfind(attrs, "href", 0)} end)
        
IO.inspect(links)
```

**Приклад виводу:**

```elixir
[{"Офіційний вебсайт Elixir", {"href", "https://elixir-lang.org/"}}, {"HexDocs", {"href", "https://hexdocs.pm/"}}]
```

Цей підхід дозволяє вам ефективно навігувати і парсити HTML-документи, роблячи завдання екстракції та маніпуляції з веб-даними простими в додатках Elixir.
