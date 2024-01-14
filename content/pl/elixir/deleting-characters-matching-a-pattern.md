---
title:    "Elixir: ________________________________________Usuwanie znaków pasujących do wzorca"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Dlaczego

Napisanie bloga o usuwaniu znaków odpowiadających wzorcom w języku Elixir może wydawać się nieco abstrakcyjne. Jednakże, istnieje wiele sytuacji, w których jest to bardzo przydatne. Na przykład, usuwanie białych znaków lub znaków specjalnych może pomóc uporządkować dane wejściowe lub usunąć zbędne informacje. Poniżej pokażemy jak w prosty sposób usunąć znaki zadanego typu w Elixir.

## Jak to zrobić

W języku Elixir, istnieje kilka sposobów usuwania znaków odpowiadających określonemu wzorcowi. Pierwszym sposobem jest użycie funkcji `String.replace/4`, która przyjmuje cztery argumenty: string wejściowy, regex, zastępujący string oraz opcje. Na przykład:

```
Elixir string = "To jest przykład tekstu 123"
String.replace(string, ~r/[0-9]/, "") # Wynik: "To jest przykład tekstu "
String.replace(string, ~r/[a-z]/, "") # Wynik: " 123"
```

Innym sposobem jest użycie funkcji `String.replace/2`, która jest wygodniejsza w użyciu gdy chcemy usunąć tylko jeden typ znaków. Na przykład:

```
Elixir string = "To jest inny przykład tekstu 123!"
String.replace(string, "!", "") # Wynik: "To jest inny przykład tekstu 123"
```

Jeśli chcemy usunąć więcej niż jeden typ znaków, warto skorzystać z funkcji `Regex.replace/3`, która pozwala na podanie kilku wzorców do usunięcia. Na przykład:

```
Elixir string = "To jest kolejny przykład - 123!"
Regex.replace(string, ~r/[^a-zA-Z ]/, "") # Wynik: "To jest kolejny przykład "
```

Wyrażenia regularne mogą być nieco trudne do zrozumienia dla początkujących, ale są bardzo przydatne w usuwaniu znaków odpowiadających określonemu wzorcowi.

## Deep Dive

Głębsze zrozumienie działania wyrażeń regularnych może być przydatne w bardziej skomplikowanych przypadkach, na przykład jeśli chcemy usunąć wszystkie znaki oprócz określonych. Wtedy możemy skorzystać ze składni negacji `[^]`, która oznacza, że wyrażenie będzie dopasowane tylko wtedy, gdy zadany wzorzec nie zostanie znaleziony. Na przykład:

```
Elixir string = "To jest przykład - 123!"
Regex.replace(string, ~r/[^a-zA-Z]/, "") # Wynik: "To jest przykład"
```

Warto również pamiętać, że wyrażenia regularne w języku Elixir są domyślnie nieczułe na wielkość liter. Jeśli chcemy, aby nasze wyszukiwanie było zależne od wielkości liter, możemy użyć opcji `i` przy wyrażeniu regularnym. Na przykład:

```
Elixir string = "To jest przykład - 123!"
Regex.replace(string, ~r/[^a-z]/i, "") # Wynik: "To jest przykład"
```

## Zobacz również

- [Dokumentacja Elixir - String.replace/4](https://hexdocs.pm/elixir/String.html#replace/4)
- [Dokumentacja Elixir - String.replace/2](https://hexdocs.pm/elixir/String.html#replace/2)
- [Dokumentacja Elixir - Regex.replace/3](https://hexdocs.pm/elixir/Regex.html#replace/3)
- [Elixir School - Regex](https://elixirschool.com/pl/lessons/basics/regex/)
- [Wikipedia - Wyrażenia regularne](https://pl.wikipedia.org/wiki/Wyra%C5%BCenia_regularne)