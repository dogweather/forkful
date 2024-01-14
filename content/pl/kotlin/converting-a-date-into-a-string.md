---
title:    "Kotlin: Konwersja daty na ciąg znaków"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest ważnym krokiem w wielu aplikacjach, zwłaszcza w tych związanych z zarządzaniem czasem. Przydatna jest, gdy chcemy wyświetlić datę w czytelny sposób lub zapisać ją do pliku tekstowego. W tym artykule pokażę, jak w prosty sposób dokonać konwersji daty na ciąg znaków w języku Kotlin.

## Jak to zrobić

Najprostszym sposobem na konwersję daty na ciąg znaków jest użycie metody "format" z klasy "SimpleDateFormat". Przykład kodu w języku Kotlin wyglądałby następująco:

```Kotlin
val date = Date()
val dateFormat = SimpleDateFormat("dd/MM/yyyy")
val dateString = dateFormat.format(date)
println(dateString) // output: "26/04/2021"
```

W tym przypadku tworzymy obiekt klasy "Date", który przechowuje aktualną datę oraz obiekt klasy "SimpleDateFormat", który określa format, w jakim chcemy wyświetlić datę. Warto zauważyć, że w przypadku języka polskiego występuje odwrócony porządek dnia i miesiąca (dd/MM/yyyy). Następnie używamy metody "format", aby dokonać konwersji daty na ciąg znaków, który następnie wyświetlamy na konsoli.

## Głębsza analiza

Klasy "Date" i "SimpleDateFormat" pochodzą z pakietu "java.util", co oznacza, że są dostępne w języku Kotlin bez konieczności importowania dodatkowych bibliotek. Jednakże, jeśli chcemy wykorzystać bardziej złożone formaty daty lub chcemy pracować z różnymi strefami czasowymi, możemy skorzystać z klasy "Calendar". Przykład kodu wykorzystujący tę klasę wyglądałby następująco:

```Kotlin
val date = Date()
val calendar = Calendar.getInstance()
calendar.time = date
val day = calendar.get(Calendar.DAY_OF_MONTH)
val month = calendar.get(Calendar.MONTH) + 1
val year = calendar.get(Calendar.YEAR)
val dateString = "$day/$month/$year"
println(dateString) // output: "26/04/2021"
```

W tym przypadku tworzymy obiekt klasy "Calendar", który zawiera metodę "getInstance()", aby uzyskać aktualną strefę czasową. Następnie ustawiamy datę na obiekt klasy "Date" i korzystając z metody "get" pobieramy odpowiednie wartości dla dnia, miesiąca i roku. Ważne jest również, aby zauważyć, że wartość dla miesiąca musimy zwiększyć o 1, ponieważ w klasie "Calendar" miesiące są indeksowane od 0. Na koniec łączymy te wartości w ciąg znaków i wyświetlamy na konsoli.

## Zobacz również

- Dokumentacja języka Kotlin dotycząca dat i czasu: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date/
- Konwersja daty na ciąg znaków w języku Java: https://www.baeldung.com/java-date-to-string
- Tutoriale i kursy związane z językiem Kotlin: https://pl.wikipedia.org/wiki/Kotlin#Zobacz_również