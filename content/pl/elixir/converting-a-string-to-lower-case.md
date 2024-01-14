---
title:                "Elixir: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Przetwarzanie ciągu znaków jest nieodłączną częścią każdego języka programowania. W Elixirze często będziesz musiał zmienić ciąg znaków na formę pisemną w małych literach, czyli na tzw. "lower case". W tym artykule dowiesz się, dlaczego warto to robić i jak można to zrobić w Elixirze.

## Jak to zrobić?

Kod w Elixirze jest nie tylko efektywny, ale także prosty w użyciu. Konwersja ciągu znaków na formę pisemną w małych literach jest tutaj bardzo prosta i wyraźna. Wystarczy użyć metody `String.downcase()` aby przekonwertować ciąg znaków.

```Elixir
string = "ELIXIR JEST FANTASTYCZNYM JĘZYKIEM PROGRAMOWANIA"
converted_string = String.downcase(string)
IO.puts converted_string
```

Tym prostym kodem zmienimy ciąg znaków na "elixir jest fantastycznym językiem programowania". Bardzo często w programowaniu będziesz potrzebować również usunąć spacje z ciągu znaków. W Elixirze możesz to zrobić używając metody `String.trim()`.

```Elixir
string = "  Więcej kodu tylko znaczy więcej możliwości  "
converted_string = String.downcase(string) |> String.trim()
IO.puts converted_string
```

Teraz nasz ciąg znaków będzie wyglądał następująco "więcej kodu tylko znaczy więcej możliwości". Jak widać, Elixir umożliwia nam wykonanie obu operacji w jednej linijce kodu.

## Przyjrzyjmy się tematowi bliżej

W Elixirze ciągi znaków są traktowane jako listy znaków, więc metoda `String.downcase()` właściwie jest skrótem dla funkcji `Enum.map()` wykonującej konwersję na każdym znaku z osobna. Dzięki temu metoda ta jest bardzo wydajna i nie ma potrzeby używania pętli, co znacznie ułatwia pracę programistom.

Warto również wspomnieć o tym, że Elixir jest językiem funkcyjnym i nie ma tutaj żadnych zmiennych, które są modyfikowane. Dlatego też przy użyciu metody `String.downcase()` nie zmieniamy oryginalnego ciągu znaków, tylko zwracamy nowy.

## Zobacz także

Jeśli jesteś zainteresowany dowiedzeniem się więcej o stringach w Elixirze, polecamy przeczytać te artykuły:

- https://elixir-lang.org/getting-started/string-patterns-and-regular-expressions.html
- https://medium.com/@coryodaniel/understanding-elixir-string-manipulation-92a6be76ab51
- https://www.tutorialspoint.com/elixir/elixir_strings.htm