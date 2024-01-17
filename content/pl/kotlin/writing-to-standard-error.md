---
title:                "Pisanie do standardowego błędu"
html_title:           "Kotlin: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

Cześć czytelnicy!

# Co i dlaczego?

Pisanie do standardowego błędu to proces informowania o błędach w programowaniu. Programiści często to robią, ponieważ chcą być świadomi występujących problemów i ich przyczyn, aby szybko je rozwiązać.

# Jak to zrobić:

```Kotlin
System.err.println("To jest przykładowy błąd.")
```

Wynik:

```
To jest przykładowy błąd.
```

# Głębsza analiza:

Istnieje wiele dlatego, dla których pisanie do standardowego błędu jest wygodną praktyką. Pierwszym powodem jest wyświetlanie wszystkich informacji o błędzie, co ułatwia znalezienie jego przyczyny. Drugim powodem jest to, że błędy są wyświetlane nawet w przypadku zakończenia programu, dzięki czemu można łatwo zauważyć problemy nawet w dużych projektach. Alternatywą jest pisanie do standardowego wyjścia, co sprawia, że błędy są trudniejsze do odnalezienia.

# Zobacz też:

- Oficjalna dokumentacja Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-writer.-standart-error.html
- Wideo na YouTube o znaczeniu pisania do standardowego błędu: https://www.youtube.com/watch?v=1L8jYQIjsSc
- Omówienie pisania do standardowego błędu w kontekście obsługi wyjątków: https://www.baeldung.com/java-printstream-standard-error