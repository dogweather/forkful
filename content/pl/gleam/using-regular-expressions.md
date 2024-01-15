---
title:                "Używanie wyrażeń regularnych"
html_title:           "Gleam: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Regular expressions są użytecznym narzędziem w programowaniu, pozwalającym na wyszukiwanie i manipulacje tekstu w sposób precyzyjny i wydajny. Mogą być przydatne w wielu różnych sytuacjach, od walidacji danych po przetwarzanie plików tekstowych.

## Jak to zrobić

Poniżej przedstawimy kilka przykładów użycia regular expressions w języku Gleam. Przykłady te są zapisane w formacie "```Gleam ... ```", więc możesz łatwo je skopiować i wypróbować w swoim własnym kodzie.

### Wyszukiwanie frazy w tekście

Aby wyszukać konkretną frazę w tekście, możemy skorzystać z funkcji `Regex.find()`. Na przykład, jeśli chcemy znaleźć wszystkie wystąpienia słowa "Gleam" w ciągu znaków, możemy użyć następującego kodu:

```Gleam
my_text = "Hello World! I love Gleam."
pattern = Regex.compile("Gleam")
matches = Regex.find(my_text, pattern)
```

Wynikiem będzie lista wszystkich indeksów w ciągu znaków, w których znajduje się szukane słowo. W tym przypadku, będzie to `[17]` ponieważ "Gleam" zaczyna się od 17 znaku.

### Walidacja danych

Regular expressions mogą być również wykorzystane do sprawdzania czy dane są poprawnego formatu. Na przykład, jeśli chcemy sprawdzić czy dany ciąg znaków jest poprawnym adresem email, możemy użyć następującego kodu:

```Gleam
email = "example@test.com"
pattern = Regex.compile("[A-Za-z0-9.-_]+@[A-Za-z0-9]+\.[A-Za-z]+")
is_valid = Regex.matches(email, pattern)
```

Wynikiem będzie wartość logiczna `true`, ponieważ email spełnia zdefiniowany przez nas wzorzec.

### Zamiana tekstu

Regular expressions umożliwiają również proste i szybkie zamiany w tekście. Na przykład, jeśli chcemy zamienić wszystkie wystąpienia słowa "Gleam" na "Gleam 2.0" w danym tekście, możemy użyć funkcji `Regex.replace()`:

```Gleam
my_text = "Gleam is the best!"
pattern = Regex.compile("Gleam")
new_text = Regex.replace(my_text, pattern, "Gleam 2.0")
```

Wynikiem będzie "Gleam 2.0 is the best!".

## Deep Dive

Regular expressions w języku Gleam są oparte na standardowej bibliotece `re`. Biblioteka ta oferuje wiele różnych funkcji, które mogą być przydatne w bardziej zaawansowanych sytuacjach. Dokumentacja biblioteki zawiera szczegółowe objaśnienia oraz przykłady użycia.

Dla tych, którzy są bardziej zaawansowani w dziedzinie regular expressions, warto również zapoznać się z różnymi składnikami składni języka Gleam, takimi jak pattern matching, funkcje wyższego rzędu oraz moduły. Mogą one być wykorzystane w połączeniu ze zwykłymi wyrażeniami regularnymi, aby stworzyć jeszcze bardziej potężne narzędzie do przetwarzania tekstu.

## Zobacz również

Dokumentacja języka Gleam: https://gleam.run/

Dokumentacja standardowej biblioteki `re`: https://gleam.run/modules/re.html

Przydatne zagadnienia dotyczące regular expressions: https://regexone.com/