---
title:    "Kotlin: Otrzymywanie aktualnej daty"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

Dlaczego:

Często w trakcie tworzenia aplikacji potrzebujemy aktualnej daty. Jest to ważne do ustawiania czasowych wydarzeń, logowania czasu w aplikacji czy też wyświetlania bieżącej daty użytkownikowi. W tym wpisie dowiesz się, jak w prosty sposób uzyskać aktualną datę w języku Kotlin.

## Jak to zrobić?

Aby uzyskać aktualną datę w języku Kotlin, możemy skorzystać z klasy `LocalDate` z pakietu `java.time`. Najpierw musimy zaimportować ten pakiet. Następnie, wewnątrz bloku "```Kotlin
... ```" możemy użyć metody `now()` na obiekcie `LocalDate`, aby uzyskać obecną datę.

```Kotlin
import java.time.LocalDate

fun main() {

    val currentDate = LocalDate.now()
    println(currentDate)

}
```

Powyższy kod wyświetli aktualną datę w formacie **yyyy-MM-dd**. Możemy także wykorzystać inne metody na obiekcie `LocalDate`, aby manipulować datami. Na przykład, aby uzyskać datę jutra, możemy skorzystać z metody `plusDays()`.

```Kotlin
val tomorrow = currentDate.plusDays(1)
println(tomorrow)
```

## Głębszy wgląd

Klasa `LocalDate` jest częścią pakietu `java.time`, który został dodany w JDK 8 i jest przeznaczony do obsługi dat i czasu w języku Java. W języku Kotlin możemy wygodnie korzystać z metod i klas z tego pakietu, dzięki czemu nie musimy samodzielnie implementować funkcjonalności dotyczących dat.

Wewnątrz klasy `LocalDate` znajduje się wiele przydatnych metod, na przykład `getDayOfWeek()`, `getMonth()` czy `getYear()`. Dzięki nim możemy pobierać konkretne wartości z daty, co jest bardzo przydatne w niektórych przypadkach.

## Zobacz także

1. Dokumentacja pakietu `java.time` - https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
2. Przewodnik po języku Kotlin - https://kotlinlang.org/docs/kotlin-docs.pdf
3. Wprowadzenie do dat i czasu w języku Java - https://www.baeldung.com/java-util-date-vs-java-time-localdate

Cieszymy się, że mogliśmy Cię wprowadzić w świat pracy z datami w języku Kotlin. Teraz możesz wykorzystać tę wiedzę w swoich projektach i tworzyć jeszcze lepsze aplikacje! Nie zapomnij zajrzeć do linków w sekcji "Zobacz także", które mogą być dla Ciebie również wartościową lekturą.