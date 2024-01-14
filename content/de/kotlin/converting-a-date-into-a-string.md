---
title:                "Kotlin: Umwandlung eines Datums in eine Zeichenfolge"
simple_title:         "Umwandlung eines Datums in eine Zeichenfolge"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Die Konvertierung eines Datums in einen String kann in vielen Fällen nützlich sein, z. B. beim Speichern von Daten in einer Datenbank oder beim Anzeigen von Datumsinformationen in einer Benutzeroberfläche.

## Wie geht man vor

Die Konvertierung eines Datums in einen String ist in Kotlin einfach und unkompliziert. Dazu kann die eingebaute Funktion `toString()` verwendet werden. Hier ist ein Beispiel:

```Kotlin
// Erstellen eines Date-Objekts mit dem aktuellen Datum
val date = Date()

// Konvertierung in einen String mit der Funktion toString()
val dateString = date.toString()

// Ausgabe des Strings
println(dateString) // Ausgabe: Thu Jan 28 14:54:25 EST 2021
```

Es ist auch möglich, das Format des Date-Strings anzupassen, indem man einen `SimpleDateFormat` verwendet. Hier ist ein Beispiel:

```Kotlin
// Erstellen eines Date-Objekts mit dem aktuellen Datum
val date = Date()

// Erstellen eines SimpleDateFormat-Objekts mit dem gewünschten Datumformat
val dateFormat = SimpleDateFormat("dd.MM.yyyy")

// Konvertierung in einen String mit der Funktion format()
val dateString = dateFormat.format(date)

// Ausgabe des Strings
println(dateString) // Ausgabe: 28.01.2021
```

## Weitere Details

Wann immer ein Datum in einen String konvertiert wird, ist es wichtig, das richtige Datumformat zu verwenden. Andernfalls kann es zu unerwarteten Ergebnissen oder Fehlern kommen. Es ist auch wichtig, dass das Datum und die Zeitzone berücksichtigt werden, um sicherzustellen, dass der konvertierte String das korrekte Datum und die korrekte Uhrzeit darstellt.

## Siehe auch

- [Date and Time Manipulation in Kotlin](https://kotlinlang.org/docs/datetime.html)
- [Java SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- [Kotlin String Formatting](https://kotlinlang.org/docs/reference/basic-types.html#string-formatting)