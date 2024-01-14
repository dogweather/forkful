---
title:                "Kotlin: Konwertowanie ciągu znaków na małe litery"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja ciągu znaków na małe litery jest bardzo przydatna w programowaniu i może pomóc nam w wielu sytuacjach. Przeczytaj ten artykuł, aby dowiedzieć się, dlaczego jest to ważna umiejętność i jak ją osiągnąć w języku Kotlin.

## Jak to zrobić

W języku Kotlin mamy do dyspozycji kilka opcji, aby przekonwertować ciąg znaków na małe litery. Najprostszą i najczęściej używaną jest metoda `toLowerCase()`. Sprawdźmy jak ją wykorzystać w praktyce:

```Kotlin
val name = "JAN KOWALSKI"
println(name.toLowerCase())
```

**Output:** jan kowalski

Możemy również skorzystać z metody `lowercase()` lub `lowercase(Locale.getDefault())` aby uwzględnić ustawienia regionalne. Przykład:

```Kotlin
val greeting = "Witaj Świecie"
println(greeting.lowercase())
println(greeting.lowercase(Locale.getDefault()))
```

**Output:** witaj świat
witaj świat

## Głębszy wgląd

Każdy znak w języku Unicode ma odpowiadającą mu dużą i małą wersję. Podczas konwersji ciągu na małe litery, metoda `toLowerCase()` korzysta z informacji o kodowaniu Unicode, aby wybrać odpowiednią wersję znaku. Dlatego też zaleca się używać tej metody zamiast standardowej metody `toLowerCase()` dostępnej w języku Java, która może nie uwzględniać pełnego zestawu znaków Unicode.

## Zobacz też

- Dokumentacja Kotlin na temat konwersji ciągów na małe litery: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html
- Przewodnik dla początkujących na temat konwersji ciągów w języku Kotlin: https://kotlinlang.org/docs/basic-types.html#strings
- Przykładowe zastosowania konwersji ciągów na małe litery: https://stackoverflow.com/questions/29210391/convert-string-to-lower-case-in-java-but-use-the-string-class