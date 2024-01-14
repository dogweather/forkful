---
title:    "Gleam: Szukanie i zastępowanie tekstu"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek miałeś/miałaś problem z znalezieniem i zamianą tekstu w swoim kodzie? Właśnie o tym będzie ta krótka i przydatna instrukcja dla Gleam, języka programowania, który ułatwia właśnie takie zadania.

## Jak to zrobić

```Gleam
let text = "Witaj świecie!"
let nowy_tekst = Gleam.String.replace(text, "świecie", "Gleam") // wynik: "Witaj Gleam!"
```

Powyższy kod pokazuje, jak prostym sposobem można znaleźć i zamienić pojedynczy wyraz w tekście. Jeśli chcesz zastosować tę funkcję na całym tekście, możesz użyć pętli `for` lub wykorzystać funkcję wbudowaną `map` do operowania na każdym elemencie w liście.

```Gleam
let lista_tekstow = ["Witaj świecie!", "Cześć Gleam!", "Programowanie jest super!"]
let nowa_lista = lista_tekstow |> Gleam.List.map(fn(t) -> Gleam.String.replace(t, "świecie", "Gleam") end) // wynik: ["Witaj Gleam!", "Cześć Gleam!", "Programowanie jest super!"]
```

Istnieje również opcja użycia wyrażeń regularnych przy zamianie tekstu. Aby to zrobić, możesz wykorzystać funkcję `regex_replace` z modułu `Regex` w Gleam.

```Gleam
import Gleam.Regex

let tekst = "Witaj świecie!"
let nowy_tekst = Gleam.Regex.regex_replace(tekst, "\\s", "-") // wynik: "Witaj-świecie!"
```

## Deep Dive

Gleam oferuje wiele funkcji do wyszukiwania i zamiany tekstu, w tym:

- `replace` - zastępuje pierwsze wystąpienie szukanego wyrazu w tekście
- `replace_all` - zastępuje wszystkie wystąpienia szukanego wyrazu w tekście
- `regex_replace` - zastępuje wszystkie wystąpienia pasujące do podanego wyrażenia regularnego
- `case_insensitive_replace` - zastępuje pierwsze wystąpienie szukanego wyrazu niezależnie od wielkości liter
- `case_insensitive_replace_all` - zastępuje wszystkie wystąpienia szukanego wyrazu niezależnie od wielkości liter

Możesz także wykorzystać opcję zamiany tekstu przy pomocy funkcji `String.foldl`, która pozwala na bardziej złożone manipulacje tekstem.

## Zobacz też

- Oficjalna dokumentacja Gleam: [https://gleam.run/](https://gleam.run/)
- Kurs Gleam: [https://funktionale-programmierung.net/gleam/](https://funktionale-programmierung.net/gleam/)
- Przykładowy projekt w Gleam: [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)