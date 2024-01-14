---
title:    "Kotlin: Zapisywanie tekstu wielką literą"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu konieczne jest zmienianie określonych wartości w stringach, takich jak nazwy użytkowników czy tytuły wpisów. Jedną z takich operacji jest zamiana pierwszych liter w każdym słowie na wielkie litery. W tym blogu dowiesz się, jak wykonać tę operację w kodzie Kotlin.

## Jak to zrobić

Wykorzystując wbudowaną funkcję <code>capitalize()</code> w obiekcie String, bardzo łatwo można zmienić pierwszą literę w stringu na wielką. Wystarczy tylko wprowadzić swoją zmienną typu String używając symbolu `.` oraz funkcji <code>capitalize()</code>. Poniżej znajduje się przykładowy kod w języku Kotlin oraz efekt działania:

```Kotlin
val name = "jan kowalski"
println(name.capitalize())
```
Output: Jan kowalski

Możesz również wykorzystać pętlę <code>for</code> i funkcję <code>replaceFirstChar</code> w celu zmiany pierwszej litery w każdym słowie na wielką, jak w poniższym przykładzie:

```Kotlin
val title = "mój pierwszy post"
var newTitle = ""
for (word in title.split(" ")) {
    newTitle += word.replaceFirstChar { if (it.isLowerCase()) it.titlecase() else it.toString() }
    newTitle += " "
}
println(newTitle.trim())
```
Output: Mój Pierwszy Post

## Deep Dive

Metoda <code>capitalize()</code> jest przydatnym narzędziem w zamianie pierwszej litery na wielką w danym stringu. Oprócz tego, można również wykorzystać funkcje takie jak <code>lowercase()</code> i <code>uppercase()</code> do zmiany wszystkich liter w stringu na małe lub wielkie. Funkcje te są wyjątkowo przydatne podczas walidacji danych wejściowych czy porównywania wartości.

## Zobacz również
- [Dokumentacja Kotlin - String.capitalize()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)
- [Funkcje String w języku Kotlin](https://www.marcinkossowski.pl/funkcje-string-w-jezyku-kotlin/)