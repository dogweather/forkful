---
title:    "Kotlin: Łączenie ciągów znaków"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach nieodłączną częścią programowania jest manipulowanie ze stringami – czyli ciągami znaków. Często zdarza się, że chcemy połączyć dwa lub więcej stringów w celu utworzenia jednego, kompletnego wyrażenia. W tym przypadku bardzo przydatne jest stosowanie tzw. "konkatenacji" stringów.

## Jak to zrobić

Język Kotlin oferuje nam kilka sposobów na konkatenację stringów. Pierwszym i najbardziej popularnym jest użycie operatora plus (`+`). Dzięki niemu możemy połączyć dwa stringi w jeden, jak pokazano poniżej:

```Kotlin
val firstString = "Hello "
val secondString = "world!"
val result = firstString + secondString
println(result) // wyświetli "Hello world!"
```

Innym sposobem jest wykorzystanie funkcji `plus()` zadeklarowanej na klasie `String`. Działanie jest podobne, ale w tym przypadku operator `+` jest zamieniany na wywołanie metody `plus()`.

```Kotlin
val firstString = "Hello "
val secondString = "world!"
val result = firstString.plus(secondString)
println(result) // wyświetli "Hello world!"
```

Możemy również użyć funkcji `stringOf()` lub `concat()` – jednak w praktyce rzadko się to zdarza, ponieważ są one mniej czytelne.

## Wnikliwa analiza

W języku Kotlin zmienne typu `String` są niemodyfikowalne (ang. immutable), co oznacza, że nie możemy zmienić ich zawartości. Dlatego też przy konkatenacji nie jest tworzony nowy obiekt typu `String`, a jedynie utworzone jest nowe wyrażenie zawierające oba stringi.

Jednak należy uważać przy konkatenacji dużej ilości stringów, ponieważ może to negatywnie wpłynąć na wydajność naszej aplikacji. W takiej sytuacji lepiej wykorzystać klasę `StringBuilder`, która została stworzona specjalnie do operacji na ciągach znaków. Przykład wykorzystania `StringBuilder` jest pokazany poniżej:

```Kotlin
val firstString = "Hello "
val secondString = "world!"
val builder = StringBuilder()
builder.append(firstString)
builder.append(secondString)
val result = builder.toString()
println(result) // wyświetli "Hello world!"
```

## Zobacz też

[Dokumentacja poprawności konkatenacji stringów w języku Kotlin](https://kotlinlang.org/docs/tutorials/kotlin-for-py/str-templates.html)

[Różnice między konkatenacją, interpolacją i wykorzystywaniem funkcji `format()`](https://proandroiddev.com/string-interpolation-vs-format-string-vs-concatenation-in-kotlin-c26aa1c01476)

[Trywialne funkcje stringowe w języku Kotlin](https://blog.kotlin-academy.com/trivial-string-functions-strings-in-kotlin-e44ab9ee2ef8?gi=7de6764c2227)