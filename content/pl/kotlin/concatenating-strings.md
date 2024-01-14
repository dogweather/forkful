---
title:                "Kotlin: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Wielu programistów często spotyka się z problemem połączenia ciągów znaków w swoim kodzie. Niezależnie od tego, czy tworzysz aplikację mobilną, czy stronę internetową, często będziesz musiał łączyć różne elementy tekstu, aby stworzyć pełne zdanie lub wyświetlić odpowiedni komunikat dla użytkownika. Właśnie dlatego poznaniu sposobu łączenia ciągów znaków w Kotlinie może być bardzo przydatne.

## Jak to zrobić

Aby połączyć dwa ciągi znaków w Kotlinie, możesz skorzystać z operatora plus (+) lub metody `plus()`.

```Kotlin
var firstName = "Jan"
var lastName = "Kowalski"

var fullName = firstName + lastName

println(fullName) // wynik: JanKowalski
```

Operator plus (+) lub metoda `plus()` umożliwiają łączenie dowolnej liczby ciągów znaków. Możesz także połączyć ze sobą zmienne i stałe, na przykład:

```Kotlin
var name = "Maria"
val age = 25

var message = "Witaj, tu " + name + ". Masz już " + age + " lat."

println(message) // wynik: Witaj, tu Maria. Masz już 25 lat.
```

Jeśli potrzebujesz łączyć ciągi znaków w różnych miejscach w swoim kodzie, warto wykorzystać funkcję `StringBuilder`. Pozwala ona na wydajniejsze łączenie ciągów, ponieważ nie tworzy nowych obiektów przy każdej operacji.

```Kotlin
val size = 5
val color = "czerwony"

val carDescription = StringBuilder("Samochód o rozmiarze ")
                    .append(size)
                    .append(" metrów i kolorze ")
                    .append(color)
                    .append(".")

println(carDescription.toString()) // wynik: Samochód o rozmiarze 5 metrów i kolorze czerwony.
```

Możesz także wykorzystać funkcję `format()` w celu sformatowania danych w ciągu znaków.

```Kotlin
val height = 170
val weight = 60.5

val bmiMessage = "Twoje BMI wynosi %.2f, a Twoja wysokość to %d cm".format(weight / (height / 100) * (height / 100), height)

println(bmiMessage) // wynik: Twoje BMI wynosi 20.91, a Twoja wysokość to 170 cm.
```

## Głębsza analiza

Podczas łączenia ciągów znaków w Kotlinie, ważne jest, aby pamiętać o wydajności kodu. Częste używanie operatora plus (+) lub metody `plus()` może znacznie obciążyć pamięć i spowolnić działanie aplikacji. Dlatego warto rozważyć użycie funkcji `StringBuilder` lub `format()` do łączenia ciągów w bardziej skomplikowanych aplikacjach.

Możesz także użyć funkcji `joinToString()` w celu połączenia elementów kolekcji w jeden ciąg znaków z określonym separatorem.

```Kotlin
val colors = listOf("czerwony", "niebieski", "zielony")

val colorsMessage = colors.joinToString(", ")

println("Dostępne kolory: $colorsMessage.") // wynik: Dostępne kolory: czerwony, niebieski, zielony.
```

Inną przydatną techniką jest wykorzystanie złożonego ciągu znaków zamiast wykonywania wielu operacji konkatenacji.

```Kotlin
val firstName = "Anna"
val lastName = "Nowak"

val fullName = "$firstName $lastName" // wykorzystanie złożonego ciągu znaków

println(fullName) // wynik: Anna Nowak
```

## Zobacz także

- [Dokumentacja Kotlina o operacji konkatenacji](https://kotlin