---
title:                "Pobieranie aktualnej daty"
html_title:           "Arduino: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Pozyskiwanie aktualnej daty w Kotlinie

## Co i dlaczego?

Pozyskiwanie aktualnej daty oznacza odczytanie bieżącej daty i godziny z systemu. Programiści robią to, aby umożliwić śledzenie zdarzeń czy logowanie aktywności.

## Jak to zrobić:

Pozyskanie aktualnej daty i godziny w Kotlinie jest bardzo proste. Oto kod pokazujący, jak to zrobić.

```Kotlin
import java.time.LocalDateTime

fun main() {
    val current = LocalDateTime.now()

    println("Aktualna data i godzina: $current")
}
```

Kiedy uruchomisz ten program, powinieneś zobaczyć wyjście podobne do tego:

```
Aktualna data i godzina: 2022-03-27T12:34:56.789
```

## Deep Dive

Historia Klausury bibljoteki java.time zaczyna się od Javy 8, gdzie została wprowadzona jako poprawka do starszych java.util.Date i java.util.Calendar. W Kotlinie możemy bezpośrednio korzystać z tych klas Javy.

Alternatywą jest użycie biblioteki Trzeciej Strony, takiej jak Joda-Time, która oferuje bogatsze API i lepszą obsługę stref czasowych.

Za kulisami, LocalDateTime.now() jest wywoływane z zegarem systemowym, co oznacza, że dla testów jednostkowych może być potrzebne użycie innego zegara.

## Zobacz też:

1. Dokumentacja joda-time: http://www.joda.org/joda-time/
2. Dokumentacja java.time: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
3. Więcej informacji o jednostkach testowych z LocalDateTime: https://blog.jetbrains.com/kotlin/2022/01/kotlin-1-5-30-released/