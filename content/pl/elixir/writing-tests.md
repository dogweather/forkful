---
title:                "Elixir: Pisanie testów"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego warto pisać testy w programowaniu Elixir?

Pisanie testów może wydawać się niepotrzebnym krokiem w procesie tworzenia oprogramowania, jednak w przypadku programowania w Elixir jest to niezbędne. Testy pozwalają na wcześniejsze wykrycie błędów, zapewnienie stabilności kodu oraz ułatwiają późniejszą modyfikację. Dzięki nim programista może mieć większą pewność, że jego kod działa poprawnie i uniknąć wielu problemów w przyszłości.

## Jak pisać testy w Elixir?

Pisanie testów w Elixir jest bardzo proste, dzięki wbudowanej w język bibliotece do testowania o nazwie ExUnit. Aby zacząć, wystarczy utworzyć nowy plik z rozszerzeniem .exs i zdefiniować w nim testy. Przykładowy kod wyglądałby następująco:

```elixir
defmodule Testy do
  use ExUnit.Case # dołączenie modułu ExUnit
  test "Dodawanie liczb" do
    assert 2 + 2 == 4 # prosty test, który sprawdza, czy dodawanie działa poprawnie
  end
end
```

W celu uruchomienia testów należy wpisać w konsoli następującą komendę:

```
mix test
```

Jeśli wszystkie testy przejdą pomyślnie, aplikacja zwróci komunikat "All 1 tests passed". W przypadku wystąpienia błędów, zostaną one wyświetlone wraz z informacją o miejscu ich wystąpienia.

## Wprowadzenie do pisania testów na głębszym poziomie

Pisanie testów w Elixir jest bardzo elastyczne i pozwala na wiele różnych podejść. Możemy testować całe moduły, pojedyncze funkcje lub nawet warunki wyjątkowe. Możemy również wykorzystać dodatkowe moduły, takie jak ExUnit.Assertions, aby sprawdzić dokładniej oczekiwane wyniki.

Ważne jest również pamiętanie o tym, że testy powinny być napisane w taki sposób, aby były łatwe do zrozumienia i utrzymania. Powinniśmy unikać testów, które są zbyt złożone lub nieczytelne, ponieważ mogą one utrudnić odnalezienie problemów w kodzie.

## Zobacz także

- [Dokumentacja Elixir](https://hexdocs.pm/elixir/1.12.0/ExUnit.html)
- [Przewodnik po pisaniu testów w Elixir](https://dev.to/zedenem/elixir-unit-testing-guide-with-exunit-36i5)
- [Szkolenie online: Testowanie w Elixir](https://www.udemy.com/course/elixir-i-phoenix-testowanie-dla-poczatkujacych/)

Dzięki testom możemy zwiększyć jakość naszego kodu oraz zaoszczędzić czas i zasoby w przyszłości. Pamiętajmy więc o pisaniu testów od samego początku tworzenia aplikacji, a nasz kod będzie bardziej niezawodny i łatwiejszy w utrzymaniu.