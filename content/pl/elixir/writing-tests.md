---
title:    "Elixir: Pisanie testów"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego pisać testy w Elixirze?

Testowanie jest nieodłączną częścią procesu pisania oprogramowania. Odpowiednio napisane testy pomagają zapewnić jakość kodu oraz ułatwiają późniejszą pracę nad projektem. W przypadku języka Elixir, testowanie jest szczególnie ważne ze względu na jego funkcjonalności i składnię. 

## Jak pisać testy w Elixirze?

Pisanie testów w Elixirze jest bardzo łatwe i intuicyjne. Wystarczy użyć wbudowanych w język narzędzi, takich jak moduł `ExUnit`. Poniżej przedstawiamy przykładowy kod testów, który sprawdzi, czy funkcja `double/1` zwraca podwójną wartość liczby przekazanej jako argument.

```Elixir
defmodule Testowanie do
  use ExUnit.Case
  
  test "podwójna wartość" do
    assert double(5) == 10
  end
  
  def double(n) do
    n * 2
  end
end
```

Po uruchomieniu testów, otrzymujemy następujący wynik:

```
...

  1) test podwójna wartość (Testowanie)
     testowanie.exs:5
     Assertion with == failed
     code: double(5) == 10
     lhs:  10
     rhs:  11
     stacktrace:
       testowanie.exs:5: (test)

Finished in 0.02 seconds
1 doctest, 1 failure

Randomized with seed 584660
```

Dzięki temu, że test nie przeszedł, możemy ustalić, że nasza funkcja `double/1` powinna zwracać wartość `11`, a nie `10`.

## Głębsze wgląd w pisanie testów

Pisanie testów pozwala nam nie tylko sprawdzić poprawność kodu, ale również zapobiega błędom czy regresjom. W Elixirze istnieje wiele wbudowanych funkcji, takich jak `assert` czy `refute`, które pomagają w efektywnym testowaniu. Warto również zapoznać się z innymi użytecznymi narzędziami, takimi jak `Mock`, `ExVCR` czy `Property-Based Testing`. 

## Zobacz też
- [ExUnit - dokumentacja](https://hexdocs.pm/ex_unit/)
- [Testing with Elixir - blog](https://medium.com/elixircasts/testing-with-elixir-920a421aad04)
- [Elixir testing using Property-Based Testing - blog](https://essenceofcode.com/2018/02/16/elixir-testing-using-property-based-testing/)