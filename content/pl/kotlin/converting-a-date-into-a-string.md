---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Kotlin: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami musimy przekonwertować datę na typ tekstowy, aby móc wyświetlić ją w czytelnej formie dla użytkownika lub zapisać w pliku. Na przykład, jeśli tworzysz aplikację kalendarza, będziesz musiał wyświetlić datę wybranego wydarzenia w formacie tekstowym. W takich przypadkach, konwersja daty na string jest niezbędna.

## Jak To Zrobić

Konwersja daty na string w języku Kotlin jest bardzo prosta. Można to zrobić na kilka sposobów, w zależności od preferencji programisty:

```Kotlin
// Tworzenie obiektu klasy Date
val date = Date()

// Wybór formatu, np. DD.MM.YYYY HH:mm:ss
val dateFormat = SimpleDateFormat("dd.MM.yyyy HH:mm:ss")

// Konwersja daty na typ tekstowy
val dateString = dateFormat.format(date)

// Wyświetlenie wyniku
println(dateString) // Output: 21.02.2021 12:30:00
```

W powyższym przykładzie użyliśmy klasy `Date` do utworzenia obiektu zawierającego aktualną datę i czas. Następnie określiliśmy format, w jakim chcemy wyświetlić datę za pomocą klasy `SimpleDateFormat`. W ostatnim kroku wywołaliśmy metodę `format` z obiektu formatu, podając jako argument nasz obiekt daty, co zwróciło nam datę w postaci tekstowej. Dzięki temu możemy wyświetlić ją lub zapisać w pliku.

Jednak nie musimy zawsze tworzyć obiektu klasy `Date` w celu konwersji daty na string. Możemy użyć również funkcji `now()` z klasy `LocalDateTime`, która zwróci aktualną datę i czas w formacie łatwym do czytania:

```Kotlin
println(LocalDateTime.now()) // Output: 2021-02-21T12:30:00.000
```

Możemy również dostosować format, używając funkcji `format` i klasy `DateTimeFormatter`. Na przykład, jeśli chcemy wyświetlić datę w formacie DD/MM/YYYY, możemy to zrobić w następujący sposób:

```Kotlin
println(LocalDateTime.now().format(DateTimeFormatter.ofPattern("dd/MM/yyyy"))) // Output: 21/02/2021
```

## Sprawdzanie Głębsze

Konwersja daty na string może być również wykonywana w przypadku, gdy chcemy zamienić datę w formacie tekstowym na obiekt klasy `Date` lub `LocalDateTime`.

```Kotlin
// Tworzenie stringa z datą
val dateString = "21/02/2021"

// Tworzenie formatu
val dateFormat = SimpleDateFormat("dd/MM/yyyy")

// Konwersja stringa na obiekt Date
val date = dateFormat.parse(dateString)

// Wyświetlenie daty
println(date) // Output: Sun Feb 21 00:00:00 CET 2021
```

Podobnie, z pomocą funkcji `parse` i klasy `DateTimeFormatter`, możemy przekonwertować stringa na obiekt `LocalDateTime`:

```Kotlin
val dateString = "21/02/2021"

// Tworzenie formatu
val dateFormat = DateTimeFormatter.ofPattern("dd/MM/yyyy")

// Konwersja stringa na obiekt LocalDateTime
val dateTime = LocalDateTime.parse(dateString, dateFormat)

// Wyświetlenie daty
println(dateTime) // Output: 2021-02-21T00:00
```

Warto również pamiętać, że w języku Kotlin istnieje wiele gotowych bibliotek, które ułatwiają pracę z datami i czasem, takich jak `java.time` oraz `java.util`. Warto zapoznać się z dokumentacją tych bibliotek, aby wykorzystać pełnię możliwości języka.

## Zobacz również

- Dokumentacja języka Kotlin