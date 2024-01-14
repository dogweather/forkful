---
title:                "Kotlin: Das aktuelle Datum erhalten"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

In der heutigen Zeit ist es oft wichtig, das aktuelle Datum zu kennen. Sei es für geschäftliche Zwecke oder um persönliche Aufzeichnungen zu machen, die genaue Zeit und Datum können eine entscheidende Rolle in unserem täglichen Leben spielen. In dieser Blog-Post werden wir uns ansehen, wie man das aktuelle Datum in Kotlin abrufen kann und welche praktischen Anwendungen es dafür gibt.

## Wie geht man vor

Um das aktuelle Datum in Kotlin abzurufen, können wir die Date-Klasse verwenden, die in der Standardbibliothek von Kotlin enthalten ist. Hier ist ein Beispielcode, der das aktuelle Datum im Format Tag/Monat/Jahr ausgibt:

```Kotlin
import java.util.Date

val currentDate = Date()
println(currentDate)
```

Die Ausgabe dieses Codes wird je nach aktuellem Datum variieren, aber ein Beispieloutput könnte "Sat Sep 18 16:09:33 CEST 2021" sein. Um das Datum in einem bestimmten Format auszugeben, können wir die SimpleDateFormat-Klasse verwenden. Hier ist ein Beispielcode, der das aktuelle Datum im Format Tag.Monat.Jahr ausgibt:

```Kotlin
import java.util.Date
import java.text.SimpleDateFormat

val currentDate = Date()
val dateFormat = SimpleDateFormat("dd.MM.yyyy")
val formattedDate = dateFormat.format(currentDate)
println(formattedDate)
```

Die Ausgabe dieses Codes ist unabhängig vom aktuellen Datum immer im Format "TT.MM.JJJJ", also zum Beispiel "18.09.2021". Dies ist besonders nützlich, wenn man das Datum in einer Datenbank speichern oder für andere Zwecke verwenden möchte.

## Tiefergehende Einblicke

Die Date-Klasse in Kotlin ist im Grunde eine Wrapperschicht um die java.util.Date-Klasse, die in Java verwendet wird. In Kotlin gibt es jedoch einige zusätzliche Funktionen, die das Arbeiten mit Datum und Zeit erleichtern. Zum Beispiel können wir ein Datum mit der "copy" Methode kopieren und einzelne Teile davon ändern, wie zum Beispiel den Monat oder das Jahr. Auch die Java 8 Time API ist in Kotlin gut integriert und bietet zusätzliche Funktionen und Klassen zum Arbeiten mit Datum und Zeit.

Es ist wichtig zu beachten, dass die Date-Klasse in Kotlin nicht thread-sicher ist. Wenn mehrere Threads auf dasselbe Datum zugreifen, kann es zu unerwünschten Ergebnissen oder sogar Fehlern führen. In solchen Fällen sollten wir Synchronisationsmechanismen verwenden, um sicherzustellen, dass das Datum korrekt abgerufen und verwendet wird.

## Siehe auch

- Offizielle Kotlin-Dokumentation zur Date-Klasse: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date
- Java 8 Time API-Dokumentation: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- Kotlin-Java-Interop: https://kotlinlang.org/docs/tutorials/interoperability-java.html