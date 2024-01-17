---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Kotlin: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Konvertieren eines Datums in einen String ist ein häufiger Prozess in der Programmierung. Es bedeutet im Grunde, ein bestimmtes Datum in ein lesbares Format zu bringen, das für den Benutzer oder andere Teile des Programms leichter zu verstehen ist. Programmierer konvertieren Daten, um die Lesbarkeit und Funktionalität ihrer Programme zu verbessern.

## Wie geht's:
Kotlin bietet verschiedene Methoden zum Konvertieren von Daten in Strings. Hier sind ein paar Beispiele:

```Kotlin 
// Aktuelles Datum in String konvertieren
val currentDate = LocalDate.now()
val dateString = currentDate.toString()
print(dateString) // Output: 2021-08-20
```

```Kotlin
// Datum in benutzerdefiniertes Format konvertieren
val date = LocalDate.of(2021, 8, 20)
val formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy")
val dateString = date.format(formatter)
print(dateString) // Output: 20.08.2021
```

```Kotlin
// String in Datum konvertieren
val dateString = "2021-08-20"
val date = LocalDate.parse(dateString)
print(date) // Output: 2021-08-20
```

## Tiefer Einblick:
Das Konzept, Datum in Strings zu konvertieren, gibt es schon seit der Anfangszeit der Programmierung. In frühen Sprachen wie Fortran, Cobol und Basic war es üblich, Datumsangaben als Zeichenketten zu speichern. Heutzutage gibt es auch alternative Methoden, um Daten in leserliche Formate zu konvertieren, wie zum Beispiel die Verwendung von Objekten wie Date oder Calendar.

Eine interessante Besonderheit in Kotlin ist, dass es sich um ein objektorientiertes und funktionales Programmierparadigma handelt. Dadurch bietet es verschiedene Wege, um ein bestimmtes Problem zu lösen. Beispielsweise können Entwickler einen funktionalen Ansatz wählen und eine reine Funktion schreiben, die ein Datum in einen String konvertiert, oder sie können eine Klasse erstellen, die diese Funktion beinhaltet.

Weiterführende Links:
- [Java API-Dokumentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html#toString())
- [Kotlin API-Dokumentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-local-date/)