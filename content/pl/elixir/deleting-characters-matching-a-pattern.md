---
title:                "Elixir: Usuwanie znaków pasujących do wzorca."
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Każdy programista wie, że czasem trzeba przeprowadzić operacje usuwania znaków, które pasują do pewnego wzorca. Jest to niezbędne przy obróbce tekstu lub danych wejściowych. W tym artykule dowiesz się, dlaczego warto poznać możliwości silnie typowanego języka programowania jakim jest Elixir oraz jak wykorzystać jego funkcje do usuwania znaków pasujących do określonych wzorców.

## Jak to zrobić

Aby usunąć znaki pasujące do wzorca, możemy użyć funkcji `String.replace/4`. Przykładem jest usunięcie białych znaków z podanego tekstu:

```Elixir
String.replace("Witaj \t w \t świecie \n Elixira", " ", "")
```

Ten kod zwróci `"WitajwświecieElixira"`. Możemy również wykorzystać wyrażenie regularne w celu bardziej zaawansowanego usuwania. Na przykład, aby usunąć liczbę z podanego tekstu, użyjemy:

```Elixir
String.replace("Elixir '2004' jest językiem  programowania stworzonym przez Ęrica \\n\\n  \n\ Bodena", ~r/\d+/, "")
```

Kod ten zwróci `"Elixir 'jest językiem programowania stworzonym przez Ęrica \\n\\n \n\ Bodena"`.

Możemy również użyć funkcji `String.replace/3`, która automatycznie usuwa wszystkie wystąpienia danego znaku lub ciągu znaków. Na przykład, aby usunąć wszystkie spacje z tekstu, możemy wykorzystać:

```Elixir
String.replace("Elixir jest świetnym językiem programowania", " ", "")
```

Kod ten zwróci `"Elixirjestświetnymjęzykiemprogramowania"`.

## Dogłębna analiza

Funkcja `String.replace/4` przyjmuje cztery argumenty: ciąg znaków wejściowych, wzorzec do przeszukania, zastępujący ciąg znaków oraz opcje. Ostatni argument określa, czy wzorzec ma być traktowany jako wyrażenie regularne lub nie. Jeśli nie podamy tej opcji, domyślnie będzie traktowany jako zwykły ciąg znaków.

Aby dokładniej poznać wyrażenia regularne, warto zapoznać się z dokumentacją Elixir. Możemy również wykorzystać narzędzia online, na przykład [regex101](https://regex101.com/) w celu testowania naszych wyrażeń.

## Zobacz również

- [Dokumentacja Elixir](https://hexdocs.pm/elixir/String.html#replace/4)
- [Narzędzie online do testowania wyrażeń regularnych](https://regex101.com/)