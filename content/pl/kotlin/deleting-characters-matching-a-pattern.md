---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "Kotlin: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Usunięcie znaków dopasowujących wzorzec oznacza usuwanie wszystkich znaków w ciągu, które pasują do określonego wzorca. Programiści często wykonują to zadanie w celu przetwarzania danych i oczyszczenia ich z niechcianych znaków.

## Jak to zrobić:
Przykładowy kod w języku Kotlin, który usuwa znaki dopasowujące wzorzec:

```Kotlin
val tekst = "To jest przykładowy tekst z cyframi: 123456789" 
val nowyTekst = tekst.replace("\\d".toRegex(), "") 
println(nowyTekst) // Wypisze: "To jest przykładowy tekst z cyframi: "

```

Możemy również wykorzystać wyrażenia regularne do określania bardziej skomplikowanych wzorców do usunięcia. Na przykład, jeśli chcemy usunąć wszystkie numery telefonów z tekstu, możemy użyć następującego kodu:

```Kotlin
val tekst = "Mój numer telefonu to: 123-456-7890"
val nowyTekst = tekst.replace("\\d{3}-\\d{3}-\\d{4}".toRegex(), "")
println(nowyTekst) // Wypisze: "Mój numer telefonu to: "

```

## Głębsze Zagadnienia:
Ściśle mówiąc, usuwanie znaków dopasowujących wzorzec jest częścią większej metody zwanej "wyrażeniami regularnymi". Wyrażenia regularne są bardzo przydatne w przetwarzaniu danych i pozwalają nam na szybkie i skuteczne znalezienie i manipulowanie tekstem. Alternatywnym sposobem usuwania znaków dopasowujących wzorzec może być użycie funkcji "removeIf" w języku Kotlin. Jednak wyrażenia regularne są bardziej wszechstronne i dostosowalne do różnych wzorców.

Implementacja usuwania znaków dopasowujących wzorzec w języku Kotlin korzysta z klasy Regex, która umożliwia wykorzystanie wyrażeń regularnych. W tym przykładzie korzystamy z metody "replace", która zastępuje dopasowane wzorce określonym ciągiem znaków.

## Zobacz również:
Sprawdź dokumentację języka Kotlin na temat wyrażeń regularnych: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/index.html 
Aby dowiedzieć się więcej o usuwaniu znaków dopasowujących wzorce, zajrzyj na stronę: https://www.regular-expressions.info/