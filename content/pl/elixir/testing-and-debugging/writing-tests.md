---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:31.347116-07:00
description: "Pisanie test\xF3w w Elixirze polega na tworzeniu zautomatyzowanych skrypt\xF3\
  w w celu weryfikacji zachowania Twojego kodu. Programi\u015Bci robi\u0105 to, aby\
  \ zapewni\u0107\u2026"
lastmod: 2024-02-19 22:04:54.225262
model: gpt-4-0125-preview
summary: "Pisanie test\xF3w w Elixirze polega na tworzeniu zautomatyzowanych skrypt\xF3\
  w w celu weryfikacji zachowania Twojego kodu. Programi\u015Bci robi\u0105 to, aby\
  \ zapewni\u0107\u2026"
title: "Pisanie test\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie testów w Elixirze polega na tworzeniu zautomatyzowanych skryptów w celu weryfikacji zachowania Twojego kodu. Programiści robią to, aby zapewnić jakość, zapobiec regresji i ułatwić refaktoryzację kodu, czyniąc proces rozwoju bardziej niezawodnym i efektywnym.

## Jak to zrobić:
Elixir używa ExUnit jako wbudowanego frameworka do testów, który jest niezwykle potężny i łatwy w użyciu. Oto podstawowy przykład:

1. Utwórz nowy plik testowy w katalogu `test` Twojego projektu Elixir. Na przykład, jeśli testujesz moduł o nazwie `MathOperations`, Twój plik testowy może być `test/math_operations_test.exs`.

```elixir
# test/math_operations_test.exs
defmodule MathOperationsTest do
  use ExUnit.Case

  # To jest prosty przypadek testowy do sprawdzenia funkcji dodawania
  test "dodawanie dwóch liczb" do
    assert MathOperations.add(1, 2) == 3
  end
end
```

Aby uruchomić swoje testy, użyj komendy `mix test` w terminalu. Jeśli funkcja `MathOperations.add/2` poprawnie dodaje dwie liczby, zobaczysz wyjście podobne do:

```
..

Finished in 0.03 seconds
1 test, 0 failures
```

Dla testów obejmujących zewnętrzne usługi lub API, możesz chcieć używać bibliotek do mockowania, takich jak `mox`, aby unikać uderzania w rzeczywiste usługi:

1. Dodaj `mox` do swoich zależności w `mix.exs`:

```elixir
defp deps do
  [
    {:mox, "~> 1.0.0", only: :test},
    # inne zależności...
  ]
end
```

2. Zdefiniuj moduł mockujący w swoim pomocniku testowym (`test/test_helper.exs`):

```elixir
Mox.defmock(HTTPClientMock, for: HTTPClientBehaviour)
```

3. Użyj mocka w swoim przypadku testowym:

```elixir
# test/some_api_client_test.exs
defmodule SomeAPIClientTest do
  use ExUnit.Case
  import Mox

  # To mówi Mox'owi, aby zweryfikował, że ten mock został wywołany zgodnie z oczekiwaniami
  setup :verify_on_exit!

  test "otrzymuje dane z API" do
    # Przygotuj odpowiedź mocka
    expect(HTTPClientMock, :get, fn _url -> {:ok, "Zmockowana odpowiedź"} end)
    
    assert SomeAPIClient.get_data() == "Zmockowana odpowiedź"
  end
end
```

Podczas uruchamiania `mix test`, ta konfiguracja pozwala izolować twoje testy jednostkowe od rzeczywistych zewnętrznych zależności, skupiając się na zachowaniu własnego kodu. Ten wzorzec zapewnia, że twoje testy działają szybko i pozostają niezawodne, niezależnie od statusu zewnętrznych usług czy łączności internetowej.
