---
title:    "Kotlin: Wyszukiwanie i wymienianie tekstu"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Dlaczego

Edycja tekstu jest jednym z najczęściej wykonywanych zadań w programowaniu. Czasami musimy też dokonać zmian w całych plikach lub projektach, co może być czasochłonne i trudne do zrealizowania manualnie. Właśnie dlatego znajomość metod wyszukiwania i zamiany tekstu w kodzie jest niezbędna dla każdego programisty.

## Jak to zrobić

W języku Kotlin istnieje wiele przydatnych narzędzi, które pomagają w wykonaniu szybkiej i precyzyjnej wyszukiwania i zamiany tekstu. Jednym z nich jest metoda `replace()` dostępna na obiektach typu String. Przykładowe wywołanie tej metody mogłoby wyglądać następująco:

```Kotlin
val originalString = "To jest przykładowy tekst"
val modifiedString = originalString.replace("przykładowy", "nowy")
println(modifiedString)

// Output: To jest nowy tekst
```

Oprócz tego, w Kotlinie istnieją też zdefiniowane wyrażenia regularne, dzięki którym możemy dokonać bardziej zaawansowanych operacji wyszukiwania i zamiany tekstu. Przykładowo, za pomocą funkcji `replaceFirst()` możemy zastąpić tylko pierwsze wystąpienie danego słowa lub wyrażenia. Poniżej przedstawiono przykład jej użycia:

```Kotlin
val originalText = "Ala ma kota i psa"
val modifiedText = originalText.replaceFirst("(kota)", "kolejnego psa")
println(modifiedText)

// Output: Ala ma kolejnego psa i psa
```

## Głębszy wybieg

W języku Kotlin możemy skorzystać również z funkcji `replaceAll()` oraz `replaceAfter()` i `replaceBefore()` do dokonywania bardziej złożonych operacji wyszukiwania i zamiany tekstu. `replaceAll()` pozwala na zastąpienie wszystkich wystąpień danego wyrażenia, natomiast `replaceBefore()` i `replaceAfter()` pozwalają na zastąpienie tekstu przed lub po określonym wyrażeniem.

Istnieje również możliwość zastosowania wielu wyrażeń regularnych jednocześnie, co pozwala na jeszcze większą elastyczność w dokonywaniu zmian w tekście. W celu pogłębienia swojej wiedzy na temat wyszukiwania i zamiany tekstu w języku Kotlin, zalecamy zapoznać się z dokumentacją oficjalną.

## Zobacz również

- Dokumentacja oficjalna języka Kotlin: https://kotlinlang.org/docs/reference/regexp.html
- Wprowadzenie do wyrażeń regularnych w Kotlinie: https://kotlinlang.org/docs/reference/regex.html