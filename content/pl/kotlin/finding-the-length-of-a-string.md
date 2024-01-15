---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Kotlin: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego i dlaczego powinniśmy się zająć znajdowaniem długości ciągu w Kotlinie? Odpowiedź jest prosta - jest to jedna z podstawowych operacji, które są wykonywane w wielu aplikacjach. Znajdowanie długości ciągu jest niezbędne do wyświetlania danych tekstowych na ekranie oraz do przetwarzania danych w programowaniu.

## Jak to zrobić

Aby uzyskać długość ciągu w Kotlinie, możemy wykorzystać funkcję `length` obiektu `String`. Przykładowy kod wyglądałby następująco:

```Kotlin
val name = "Adam"
println(name.length) // 4
```

Powyższy kod stworzy zmienną `name` zawierającą ciąg "Adam" i następnie wyświetli jego długość na ekranie. Zamknięcie ciągu wewnątrz nawiasów pozwala na wykorzystanie funkcji `length` do zwrócenia długości tej zmiennej.

Aby uzyskać długość ciągu bezpośrednio z wprowadzonego przez użytkownika tekstu, możemy wykorzystać funkcję `readLine`, która pozwala na odczytanie danych wprowadzonych przez użytkownika. Przykładowy kod wyglądałby następująco:

```Kotlin
println("Podaj dowolne zdanie:")
val input = readLine()
println("Długość podanego zdania to ${input.length}")
```

Powyższy kod najpierw wyświetli użytkownikowi informację o wprowadzeniu zdania, a następnie wykorzysta funkcję `readLine` do odczytania wprowadzonych danych do zmiennej `input`. Następnie wyświetli długość podanego zdania, wykorzystując funkcję `length`.

## Wnikliwa analiza

Podczas pobierania długości ciągu, Kotlin używa funkcji `length` do zwrócenia ilości znaków w ciągu. Jednak w przypadku niestandardowych znaków, czy wartości puste, wynik może być inny niż oczekiwany. Dlatego ważne jest, aby w kodzie uwzględnić te możliwe scenariusze i odpowiednio z nimi sobie radzić.

Inną ciekawą funkcją jest `isEmpty`, która pozwala na sprawdzenie, czy ciąg jest pusty czy nie. Może to być przydatne, gdy potrzebujemy wykonać jakieś działania tylko w przypadku, gdy użytkownik faktycznie wprowadził dane.

## Zobacz też

- [Dokumentacja Kotlin o typie String](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Funkcje standardowe Kotlin dla typu String](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/index.html)