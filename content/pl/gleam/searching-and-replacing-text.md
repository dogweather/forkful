---
title:    "Gleam: Wyszukiwanie i zastępowanie tekstu"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Dlaczego

Czasem w trakcie pisania kodu zdań zdań z [jezyczeń](https://pl.wikipedia.org/wiki/J%C4%99zyk_(informacja)) potrzebny jest program do edycji tekstu. Może to być chociażby zmiana wszystkich nazw zmiennych, lub usunięcie zbędnych spacji czy znaków specjalnych. W takich sytuacjach nie musisz się już dłużej męczyć ręcznym wykonywaniem tych czynności, dzięki Gleam masz dostęp do narzędzia do wyszukiwania i zamiany tekstu.

## Jak to zrobić

Aby użyć funkcji szukania i zamiany tekstu w Gleam, wystarczy użyć funkcji `String.replace`, która przyjmuje następujące argumenty:

- `text` - tekst, w którym chcemy przeprowadzić zmiany;
- `search` - tekst, który chcemy wyszukać;
- `replacement` - tekst, którym chcemy zastąpić znalezione fragmenty.

Przykładowe użycie wyglądałoby tak:

```Gleam
text = "Witajcie, programiści!"
search = "Witajcie"
replacement = "Cześć"
String.replace(text, search, replacement)
```

Rezultatem powyższego kodu będzie `"Cześć, programiści!"`.

## Głębsze zanurzenie się

Funkcja `String.replace` ma również opcjonalny argument `count`, który określa maksymalną liczbę wystąpień, jakie chcemy zastąpić. Jeśli nie podamy tej wartości, zostanie użyta domyślna wartość -1, co oznacza, że zostaną zastąpione wszystkie wystąpienia tekstu.

Ponadto, możemy użyć wyrażeń regularnych do bardziej skomplikowanych operacji wyszukiwania i zamiany tekstu. Funkcja `Regex.replace_all` może być użyta w celu zamiany wystąpień wyrażeń regularnych w tekście.

## Zobacz także

- Dokumentacja Gleam dotycząca funkcji `String.replace`: https://gleam.run/std/string/#replace
- Poradnik dotyczący wyrażeń regularnych w Gleam: https://gleam.run/lib/regex/