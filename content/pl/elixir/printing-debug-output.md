---
title:    "Elixir: Wydrukowanie wyników debugowania"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie jest wyzwaniem, które wymaga nieustannego doskonalenia swoich umiejętności. Jednym ze sposobów na poprawę jakości kodu jest używanie funkcji do debugowania. W tym artykule dowiesz się, dlaczego warto korzystać z wydruku debugowania w Elixirze.

## Jak to zrobić

Wykorzystanie wydruku debugowania w Elixirze jest bardzo proste i może znacznie usprawnić proces tworzenia oprogramowania. Aby zacząć, dodaj "IO.inspect()" do swojego kodu, który jest miejscem, w którym chcesz wydrukować wartości zmiennych. Następnie uruchom swoją aplikację i sprawdź, czy wydruk został poprawnie wyświetlony w konsoli.

```elixir
defmodule Opgblog.PostController do
  def index(conn, _params) do
    posts = Repo.all(Post)

    IO.inspect(posts)

    conn
    |> render("index.html", posts: posts)
  end
end
```

Przykładowy output:

```elixir
[debug] QUERY OK source="posts" db=6.0ms
SELECT p0.`id`, p0.`title`, ...
FROM `posts` AS p0 []
[
  %Post{
    __meta__: #Ecto.Schema.Metadata<
      :loaded, "posts",
      "posts.id=0", {:conn, #PID<0.1086.0>, :...}
      ...>,
    created_at: ~U[2022-01-01 00:00:00.000000Z],
    id: 0,
    title: "First Post"
  },
  %Post{
    __meta__: #Ecto.Schema.Metadata<
      :loaded, "posts",
      "posts.id=1", {:conn, #PID<0.1086.0>, :...}
      ...>,
    created_at: ~U[2022-01-02 00:00:00.000000Z],
    id: 1,
    title: "Second Post"
  }
]
```

## Głębszy wgląd

Wydruk debugowania może również pomóc w zrozumieniu i naprawianiu błędów w aplikacji. Aby zobaczyć, które linie kodu są wykonywane, możesz dodać "IO.inspect(__ENV__)" i "IO.inspect(__MODULE__)" przed kodem, który chcesz sprawdzić. Dzięki temu, będziesz miał lepszy wgląd w to, które linie kodu są odpowiedzialne za dany output.

```elixir
defmodule Opgblog.PostController do

  IO.inspect(__ENV__)

  IO.inspect(__MODULE__)

  ...rest of the code
end
```

Przykładowy output:

```elixir
[
  line: 2,
  file: "lib/opgblog/post_controller.ex",
  function: :index,
  module: Opgblog.PostController
]
```

## Zobacz także

- [Dokumentacja Elixir](https://hexdocs.pm/elixir/IO.html#inspect/1) - oficjalna dokumentacja dla funkcji IO.inspect().
- [Blog Elixir Bubble](https://elixirbubble.io/debugging-with-io-inspect-in-elixir) - artykuł na temat debugowania w Elixirze przy użyciu IO.inspect().