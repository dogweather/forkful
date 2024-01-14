---
title:    "Kotlin: Wycinanie podciągów"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

# Dlaczego warto używać wyodrębniania podciągów w Kotlinie?

Wyodrębnianie podciągów jest nieodłączną częścią wielu zadaniach programistycznych. W języku Kotlin, dzięki wygodnej funkcji `substring()`, jest to zadanie nie tylko prostsze, ale także znacznie bardziej czytelne. Pozwala ona na wygodne wyciąganie wybranych fragmentów tekstu, co jest bardzo przydatne w wielu scenariuszach programistycznych.

## Jak to zrobić?

Używanie funkcji `substring()` jest bardzo proste. Wystarczy podać jej dwa argumenty: początkowy i końcowy indeks, który określa zakres tekstu, który chcemy wyodrębnić. Przykład:

```Kotlin
val text = "To jest przykładowy tekst."
val substring = text.substring(3, 10)
println(substring)

```

Output:
```
jest przykł
```

W powyższym przykładzie, używając funkcji `substring()`, wyodrębniliśmy fragment tekstu od trzeciego do dziesiątego indeksu i przypisaliśmy go do zmiennej `substring`. Następnie, wypisaliśmy go przy użyciu funkcji `println()`.

Jeśli potrzebujemy wyodrębnić fragment tekstu od początku, możemy pominąć pierwszy parametr i podać tylko drugi, określający końcowy indeks, np. `text.substring(0, 5)` wyodrębni pierwsze 5 znaków ze zmiennej `text`.

Funkcja `substring()` może również przyjmować tylko jeden argument - początkowy indeks, wtedy wyodrębniony zostanie cały tekst od tego indeksu do końca.

```Kotlin
val substring = text.substring(7)
println(substring)
```

Output:
```
jest przykładowy tekst.
```

## Wnikliwszy opis

Funkcja `substring()` wykorzystuje indeksowanie zaczynające się od zera - pierwszy znak tekstu posiada indeks 0, kolejny 1, i tak dalej. Zwróćmy uwagę, że końcowy indeks nie jest wliczany w wyodrębniony fragment tekstu.

Ponadto, jeśli podane indeksy wykraczają poza długość tekstu, program wyrzuci błąd `IndexOutOfBoundsException`.

Funkcja `substring()` może być również używana na obiektach typu `String?` (String z dodanym znakiem zapytania oznacza, że może to być również wartość null). Wtedy, jeśli wartość jest null, funkcja również zwróci null.

## Zobacz też
- Dokumentacja funkcji `substring()` w języku Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/substring.html
- Przykładowe zastosowania wyodrębniania podciągów w Kotlinie: https://www.programiz.com/kotlin-programming/substring