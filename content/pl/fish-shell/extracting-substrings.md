---
title:                "Fish Shell: Wycinanie podciągów"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego?

Jeśli kiedykolwiek potrzebowałeś wyodrębnić fragment tekstu z większego ciągu znaków, wiesz, jak frustrujące może to być. Szczególnie, gdy jest to potrzebne w obrębie skryptu lub programu pisząc własne pisanie od podstaw.

## Jak to zrobić?

W Fish Shell istnieje wiele sposobów na wyodrębnienie fragmentów tekstu. Jednym z najprostszych sposobów jest użycie funkcji subst (substring) wraz ze znakiem podkreślenia, aby wskazać indeks początkowy i końcowy fragmentu, który chcesz wyodrębnić. Na przykład:

```
set text "To jest przykładowy tekst"
echo $text
To jest przykładowy tekst

echo $text[5..12]
jest przykładowy
```
W pierwszej linii tworzymy zmienną zawierającą nasz tekst. Następnie wypisujemy cały tekst za pomocą polecenia echo. W ostatniej linii używamy funkcji subst (substring) i podajemy zakres indeksów od 5 do 12, co pozwala nam wyodrębnić fragment "jest przykładowy" z naszego tekstu.

Możesz również używać funkcji subst do zastępowania ciągów znaków w tekście. Na przykład:

```
set text "Myślę, że lubię pisać"
echo $text
Myślę, że lubię pisać

echo $text[13..-1]
pisać

set text $text[0..7]"kochać pisać"
echo $text
Myślę, że kochać pisać
```

Oprócz tego, Fish Shell oferuje również inne funkcje pozwalające na wygodne manipulowanie fragmentami tekstu, takie jak contains (sprawdzanie czy tekst zawiera określony ciąg znaków) czy replace (zastępowanie pewnych fragmentów tekstu innymi).

## Głębszy zanurzenie

Wykorzystywanie funkcji subst w Fish Shell może być przydatne w wielu sytuacjach, szczególnie podczas pisania skryptów lub tworzenia własnych programów. Dzięki temu narzędziu możesz precyzyjnie wyodrębnić i manipulować fragmentami tekstu, co może znacznie ułatwić i przyspieszyć Twoją pracę.

## Zobacz także

- Oficjalna dokumentacja Fish Shell: https://fishshell.com/docs/current
- Przewodnik po funkcjach Fish Shell: https://fishshell.com/docs/current/commands.html#commands
- Przykłady użycia funkcji subst: https://stackoverflow.com/questions/31738169/how-to-extract-substring-in-fish-using-index