---
title:                "Interpolacja ciągu znaków"
html_title:           "Kotlin: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Interpolowanie ciągów tekstowych w Kotlinie to metoda umożliwiająca łączenie fragmentów tekstu z różnych źródeł w celu utworzenia finalnego ciągu tekstowego. Programiści wykorzystują tę technikę, aby uprościć tworzenie i edycję ciągów tekstowych, unikając jednocześnie potrzeby tworzenia wielu zmiennych.

## Jak to zrobić:

```Kotlin
val imie = "Jan"
val wiek = 30

val powitanie = "Cześć, mam na imię $imie i mam $wiek lat."
println(powitanie)
```

**Wyjście:**
>Cześć, mam na imię Jan i mam 30 lat.

## Głębszy wgląd:

Interpolowanie ciągów tekstowych jest popularną techniką używaną przez programistów już od dawna. Początkowo, w języku C, ta metoda nazywana była "zastąpieniem znacznika" i wymagała złożonej składni.

Alternatywnym rozwiązaniem dla interpolowania ciągów tekstowych w Kotlinie jest wykorzystanie operatora plus (+) do łączenia różnych ciągów. Jednakże, interpolowanie jest preferowane ze względu na czytelniejszą i prostszą składnię.

Wewnętrznie, operacja interpolacji ciągu tekstowego w Kotlinie wykorzystuje klasę `StringBuilder`, która jest bardziej wydajnym sposobem tworzenia i łączenia ciągów tekstowych.

## Zobacz również:

- Dokumentacja Kotlina na temat interpolacji ciągów tekstowych: <https://kotlinlang.org/docs/strings.html#string-interpolation>
- Przykłady kodów z interpolacją ciągów tekstowych: <https://github.com/Kotlin/kotlin-examples/tree/master/functional/src/main/kotlin/functional>