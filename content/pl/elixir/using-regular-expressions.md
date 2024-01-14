---
title:                "Elixir: Używanie wyrażeń regularnych"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego?

Regularne wyrażenia są nieodłączną częścią każdego języka programowania, w tym także Elixira. Pozwalają one na wykonywanie zaawansowanych operacji na tekście, takich jak wyszukiwanie, podstawianie czy sprawdzanie poprawności. Dzięki nim możemy manipulować napisami w sposób szybszy i bardziej precyzyjny. Zapraszam do zapoznania się z poniższym artykułem, aby dowiedzieć się więcej na temat korzystania z regularnych wyrażeń w Elixirze.

## Jak to zrobić?

Aby rozpocząć pracę z regularnymi wyrażeniami w Elixirze, wystarczy skorzystać z podstawowego modułu `Regex`. Poniżej znajduje się przykład kodu, który wykorzystuje wyrażenie regularne do wyciągnięcia numeru telefonu z tekstu i zwrócenia go jako string:

```Elixir
phone_number = Regex.run(~r/[0-9]{3}-[0-9]{3}-[0-9]{4}/, "Moje numer telefonu to 123-456-7890.")
IO.puts(phone_number) # Output: 123-456-7890
```

Możemy również używać wyrażeń regularnych wraz z innymi funkcjami, takimi jak `String.replace/3` lub `Enum.filter/2`, aby wykonać bardziej zaawansowane operacje na tekście.

## Głębszy wgląd

W Elixirze wyrażenia regularne wykorzystują składnię wyrażeń regularnych Perl, co oznacza, że wyrażenie musi być ujęte w `~r` i `~r{}`. Dodatkowo, moduł `Regex` oferuje wiele różnych funkcji do wykonywania operacji na wyrażeniach regularnych, takich jak `Regex.run/2` czy `Regex.scan/3`. Jeśli chcemy wykonać operacje na wyrażeniu regularnym na całym tekście, możemy skorzystać z funkcji `Regex.named_captures/2` do znalezienia wszystkich pasujących grup w tekście.

## Zobacz też

- [Oficjalna dokumentacja Elixira na temat wyrażeń regularnych](https://hexdocs.pm/elixir/Regex.html)
- [Poradnik dla początkujących w programowaniu z wykorzystaniem wyrażeń regularnych w Elixirze](https://medium.com/@angelika314/regular-expressions-in-elixir-an-idiomatic-introduction-f027f166a53)
- [Wideo tutorial na temat wyrażeń regularnych w Elixirze](https://www.youtube.com/watch?v=U4yJ1TOQax0)