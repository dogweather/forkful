---
title:    "Kotlin: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Dlaczego

Kalkulowanie daty w przeszłości lub przyszłości może być bardzo przydatne w wielu sytuacjach w programowaniu. Może być to potrzebne, aby ustawić przyszłe wydarzenia w aplikacji lub obliczyć wiek danej osoby na podstawie daty urodzenia. W tym artykule dowiesz się, jak wykorzystać Kotlin do przeprowadzenia takich obliczeń.

## Jak to zrobić

Aby wykonać obliczenia związane z datami w przyszłości lub przeszłości, musimy najpierw zaimportować bibliotekę `java.time.LocalDate` w naszym kodzie:

```Kotlin
import java.time.LocalDate
```

Następnie, aby obliczyć datę w przyszłości, musimy utworzyć obiekt `LocalDate` i dodać odpowiednią liczbę dni, tygodni, miesięcy lub lat do bieżącej daty:

```Kotlin
val dzisiaj = LocalDate.now() // bieżąca data
val dataWPrzyszlosci = dzisiaj.plusDays(7) // dodanie 7 dni do bieżącej daty
```

W ten sam sposób możemy obliczyć datę w przeszłości, dodając ujemną wartość do bieżącej daty:

```Kotlin
val dataWPrzeszlosci = dzisiaj.minusMonths(6) // odejmowanie 6 miesięcy od bieżącej daty
```

Możemy również używać różnych jednostek, takich jak `ChronoUnit.DAYS`, `ChronoUnit.WEEKS` czy `ChronoUnit.YEARS`, aby dostosować nasze obliczenia.

## Pogłębiona analiza

Standardowa biblioteka języka Java `java.time` oferuje wiele przydatnych klas i metod do wykonywania obliczeń związanych z datami. Przykładowe operacje, które możemy wykonać, to sprawdzanie, czy dany rok jest przestępny, porównywanie dwóch dat czy obliczanie różnicy między nimi.

Kotlin także oferuje wbudowaną funkcję `in`, dzięki której możemy sprawdzić, czy dana data znajduje się między ustalonym przedziałem czasu:

```Kotlin
val data = LocalDate.parse("2021-09-10")
if (data in LocalDate.of(2021, Month.SEPTEMBER, 1)..LocalDate.of(2021, Month.SEPTEMBER, 30)) {
    println("Data znajduje się we wrześniu")
}
```

Możemy także formatować i wyświetlać daty w różnych formatach za pomocą funkcji `format` i `parse`:

```Kotlin
val data = LocalDate.now()
val format = DateTimeFormatter.ofPattern("dd.MM.yyyy")
println(data.format(format)) // wyświetli datę w formacie "dd.MM.yyy" (np. 05.09.2021)
```

## Zobacz też

* [Dokumentacja klasy LocalDate w języku Java](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html)
* [Tutorial "Dziedziny czasu w języku Java" (ang.)](https://www.baeldung.com/java-chrono-unit)