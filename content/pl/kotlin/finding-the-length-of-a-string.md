---
title:                "Kotlin: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego
Znalezienie długości ciągu znaków jest niezbędnym narzędziem w każdym języku programowania, a w Kotlinie jest to szczególnie proste i intuicyjne. Dowiedz się, dlaczego warto poznać tę funkcjonalność i jak z niej korzystać.

## Jak to zrobić
Aby znaleźć długość ciągu znaków w Kotlinie, należy użyć metody "length". Przykładowy kod wyglądałby następująco:

```Kotlin
val name = "Kotlin"
println(name.length)
```
W powyższym przykładzie za pomocą metody "length" wyświetlamy długość zmiennej "name", czyli 6. Możemy także wyświetlić długość ciągu znaków bez użycia zmiennej, np.:

```Kotlin
println("Programowanie w Kotlinie".length)
```

Wynik tego kodu również wynosi 6, ponieważ liczone są tu znaki, a nie słowa.

## Głębszy wgląd
Chociaż funkcja "length" jest prostym i niezwykle przydatnym sposobem na znalezienie długości ciągu znaków, warto zwrócić uwagę na kilka rzeczy. Po pierwsze, metoda ta jest wywoływana na zmiennej typu String, czyli tekstu. Jeśli spróbujemy użyć jej na zmiennej innego typu, otrzymamy błąd.

Po drugie, zauważyliśmy, że w przykładowym kodzie użyliśmy funkcji "println" do wyświetlenia wyniku. Możemy również przypisać długość ciągu znaków do innej zmiennej lub wykorzystać ją w dalszych operacjach.

## Zobacz także
Jeśli chcesz pogłębić swoją wiedzę o Kotlinie, koniecznie zajrzyj na stronę dokumentacji języka oraz wypróbuj inne funkcje i metody. Możesz także zapoznać się z innymi językami programowania i porównać różnice. Poniżej kilka przydatnych linków:

- [Oficjalna strona języka Kotlin](https://kotlinlang.org/)
- [Dokumentacja języka Kotlin](https://kotlinlang.org/docs/home.html)
- [Kurs programowania w Kotlinie na Codecademy](https://www.codecademy.com/learn/learn-kotlin)
- [Porównanie języka Kotlin z innymi popularnymi językami programowania](https://www.raywenderlich.com/4738-kotlin-vs-java-comparison-of-top-features)