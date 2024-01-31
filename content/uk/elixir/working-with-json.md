---
title:                "Робота з JSON"
date:                  2024-01-19
html_title:           "Arduino: Робота з JSON"
simple_title:         "Робота з JSON"

category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Що та чому?
JSON - формат обміну даними. Програмісти використовують JSON, тому що він легкий, читабельний та широко підтримується мовами програмування, у тому числі Elixir.

## Як це зробити:
Встановлюємо бібліотеку `jason`:

```elixir
# Додайте в mix.exs
defp deps do
  [
    {:jason, "~> 1.2"}
  ]
end
```

Серіалізація (з Elixir у JSON):

```elixir
# Код
map = %{name: "Олексій", age: 28}
json = Jason.encode!(map)
IO.puts json

# Вивід
{"age":28,"name":"Олексій"}
```

Десеріалізація (з JSON у Elixir):

```elixir
# Код
json = "{\"name\":\"Олексій\",\"age\":28}"
map = Jason.decode!(json)
IO.inspect map

# Вивід
%{"age" => 28, "name" => "Олексій"}
```

## Поглиблене занурення:
Historically, JSON (JavaScript Object Notation) evolved from the need for a stateless, easy-to-parse data interchange format. It replaced XML in many applications for its simplicity. In Elixir, the `jason` library is a popular choice, though alternatives like `poison` and `jsx` exist. While `jason` prioritizes performance and compliance with the JSON standards, alternatives might offer different trade-offs.

Технічно, працюючи з JSON в Elixir, ми маємо справу з трансформацією еліксирських структур даних в строки JSON та назад. Elixir використовує протоколи, як `Jason.Encoder`, для серіалізації структур, і подає красиві помилки, коли не може серіалізувати невідомі типи.

## Дивіться також:
- [HexDocs Jason Library](https://hexdocs.pm/jason/Jason.html)
