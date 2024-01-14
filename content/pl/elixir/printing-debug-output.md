---
title:                "Elixir: Dr"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Wiele razy podczas pisania kodu w Elixirze, możemy napotkać trudne do znalezienia błędy lub zagadki dotyczące stanu naszej aplikacji. W takich sytuacjach pomocne może być wypisywanie informacji do konsoli w celu zrozumienia, co się dzieje w naszym programie. W tym artykule dowiesz się, dlaczego warto korzystać z wypisywania debugowych informacji oraz jak to zrobić w kodzie Elixir.

## Jak to zrobić

Aby wypisywać debugowe informacje w Elixirze, możemy skorzystać z funkcji `IO.inspect/2`. Możemy przekazać do niej dowolny obiekt i zostanie on wyświetlony w konsoli na etapie kompilacji lub działania naszego kodu. Przykładowy kod wyglądałby następująco:

```Elixir
defmodule User do
  defstruct name: "", age: 0

  def display_name(user) do
    IO.inspect(user.name)
  end
end

user = %User{name: "Adam", age: 32}
User.display_name(user)

# Output:
# "Adam"
```

Możemy również przekazać drugi parametr do funkcji `IO.inspect/2`, który pozwala na określenie, czy obiekt powinien zostać wyświetlony z dodatkowymi informacjami, takimi jak linia kodu, w której został wywołany. Przykładowy kod będzie wyglądał następująco:

```Elixir
defmodule User do
  defstruct name: "", age: 0

  def display_age(user) do
    IO.inspect(user.age, label: "User age", pretty: true)
  end
end

user = %User{name: "Adam", age: 32}
User.display_age(user)

# Output:
# User age: 32
# Line: 7
```

Pamiętaj, że funkcja `IO.inspect/2` nie jest jedynym sposobem na wypisywanie debugowych informacji w Elixirze. Istnieje wiele innych narzędzi, takich jak `Logger` czy `ExUnit`, które również mogą być przydatne podczas debugowania kodu.

## Deep Dive

Jeśli chcesz poznać więcej szczegółów na temat wypisywania debugowych informacji w Elixirze, warto zagłębić się w dokumentację języka. Możesz znaleźć tam jeszcze więcej funkcji, które pomogą Ci w wyświetlaniu i analizowaniu stanu swojego programu.

## Zobacz również

- [Dokumentacja Elixir - funkcja IO.inspect/2](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [Narzędzia do debugowania w Elixirze](https://elixir-lang.org/getting-started/debugging.html)