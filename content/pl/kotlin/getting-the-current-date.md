---
title:                "Kotlin: Uzyskiwanie bieżącej daty"
simple_title:         "Uzyskiwanie bieżącej daty"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Dlaczego warto poznać bieżącą datę

Poznawanie bieżącej daty jest niezbędną umiejętnością w programowaniu, ponieważ pozwala nam na tworzenie aplikacji i funkcji, które wykorzystują aktualny czas. Dzięki temu możemy tworzyć narzędzia, które są bardziej interaktywne i dokładne, dostarczając użytkownikom aktualne informacje lub reagując na określone zdarzenia w określonym czasie.

# Jak to zrobić

Aby uzyskać bieżącą datę w języku Kotlin, możemy użyć wbudowanej klasy `LocalDateTime`. Musimy najpierw zaimportować tę klasę za pomocą `import java.time.LocalDateTime`, a następnie możemy wywołać metodę `now()` wewnątrz wyrażenia `LocalDateTime`:

```Kotlin
val currentDate = LocalDateTime.now()
```

Możemy również wyświetlić bieżącą datę za pomocą interaktywnego polecenia `println`:

```Kotlin
println("Bieżąca data to: $currentDate")
```

W ten sposób wyświetli się aktualna data w formacie `YYYY-MM-DD HH:MM:SS`.

# Deep Dive

Aby lepiej zrozumieć jak działa uzyskiwanie bieżącej daty w języku Kotlin, warto zapoznać się z `LocalDateTime` oraz innymi klasami z pakietu `java.time`. Ta biblioteka została wprowadzona wraz z wersją Javy 8, aby zastąpić starą klasę `Date`, która miała wiele problemów, w tym związanych z wyświetlaniem i obliczaniem stref czasowych.

`LocalDateTime` zapewnia nam dokładność do nanosekund oraz umożliwia przetwarzanie bieżącej daty i czasu za pomocą różnych metod, takich jak `plusDays()`, `minusMonths()` itp. Możemy również zmienić format wyświetlania daty za pomocą `DateTimeFormatter` oraz przekształcić datę na obiekt `Date` za pomocą `DateConverter`.

# Zobacz także

- Dokumentacja pakietu `java.time`: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- Tutoriale dotyczące biblioteki `java.time`: https://www.baeldung.com/java-8-date-time-intro