---
title:    "Kotlin: Konwersja ciągu znaków na małe litery"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Dlaczego

W programowaniu często musimy pracować z tekstem, a właśnie ta umiejętność jest jedną z kluczowych w pracy z tekstami. W tym przewodniku dowiemy się jak w języku Kotlin przekonwertować tekst na małe litery.

## Jak to zrobić

Aby przekonwertować tekst na małe litery w języku Kotlin, musimy użyć metody `toLowerCase()`.

Przykład:
```kotlin
val tekst = "KOTLIN TO NAPRAWDĘ FANTASTYCZNY JĘZYK!"
val nowyTekst = tekst.toLowerCase()
println(nowyTekst)
```

**Output:** `kotlin to naprawdę fantastyczny język!`

Możemy również wykorzystać metodę `toLowerCase()` w celu ignorowania polskich znaków diakrytycznych:

```kotlin
val tekst = "Północna Europa to ŁADNY, ale TEŻ TROCHĘ ZIMNY region."
val nowyTekst = tekst.toLowerCase(Locale("pl"))
println(nowyTekst)
```

**Output:** `północna europa to ładny, ale też trochę zimny region.`

## Głębsza analiza

Wiemy już jak prosto przekonwertować tekst na małe litery, ale warto odkryć jak dokładnie działa ta metoda. W języku Kotlin, ciąg znaków jest reprezentowany przez klasę `String`, która dostarcza nam również metody do manipulacji tekstem.

Metoda `toLowerCase()` wywołuje wewnętrznie metodę `toLowerCase()` klasy `String`, która z kolei wywołuje metodę `toLowerCase(Locale.getDefault())`. Wykorzystując metodę `toLowerCase(Locale)` możemy wybrać wymagany język do konwersji tekstów.

## Zobacz także

- [Oficjalna dokumentacja języka Kotlin](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Przetwarzanie tekstów w języku Kotlin](https://www.tutorialspoint.com/kotlin/kotlin_strings.htm)
- [Kotlin for Android - przewodnik dla początkujących](https://developer.android.com/kotlin)