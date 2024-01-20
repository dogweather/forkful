---
title:                "Drukowanie komunikatów debugowania"
html_title:           "Haskell: Drukowanie komunikatów debugowania"
simple_title:         "Drukowanie komunikatów debugowania"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

# Printing Debug Output w Elixir: Podręcznik

## Co To Jest & Dlaczego To Robimy?

Drukowanie debug output oznacza wyświetlanie informacji programu w trakcie jego działania. Programiści to robią, aby łatwiej zrozumieć, co się dzieje "pod maską" i szybciej rozwiązywać problemy.

## Jak to Zrobić:

Elixir oferuje kilka metod do drukowania debug output. Poniżej przedstawiam najpopularniejsze z nich:

Pierwszym jest `IO.inspect`. Umożliwia drukowanie wartości zmiennych podczas przetwarzania listy.

```Elixir
lista = [1, 2, 3, 4]
lista
|> Enum.map(&IO.inspect/1)
|> Enum.sum()
```
W wyniku otrzymamy:
```Elixir
1
2
3
4
```

Kolejnym sposobem jest użycie `IO.puts/2`, które drukuje stringi z dodatkowym końcem linii.

```Elixir
IO.puts("Hello, świecie!")
```

Co daje:

```Elixir
Hello, świecie!
```

## Pogłębione Zagadnienia

Historia drukowania debug output ma swoje korzenie w początkach programowania, kiedy jedyne narzędzia, które mieliśmy do zrozumienia, co się dzieje z naszym kodem, były tekturowe karty dziurkowane lub drukarki.

Alternatywą dla debug output w Elixir jest użycie narzędzi do debugowania, takich jak IEx.pry lub debugger wbudowany w Erlang. Te narzędzia są bardziej zaawansowane i pozwalają na pełną kontrolę procesem debugowania, ale są też trudniejsze w użyciu.

Pod maską, funkcje `IO.puts/2` i `IO.inspect/1` korzystają z modułu `:io` z Erlang, aby skomunikować się z terminalem i wydrukować wiadomość. Wszystko to jest możliwe dzięki temu, że Elixir jest zbudowany na podstawie Erlang i może korzystać z jego funkcji.

## Zobacz Również

1. Oficjalna dokumentacja Elixir `IO` module: https://hexdocs.pm/elixir/IO.html
2. Artykuł o debugowaniu w Elixir: https://medium.com/elixirlabs/debugging-techniques-in-elixir-language-2a04fd2ce6ca
3. Dokumentacja na temat debuggera Erlang: http://erlang.org/doc/man/debugger.html