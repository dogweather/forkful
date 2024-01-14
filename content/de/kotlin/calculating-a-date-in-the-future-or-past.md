---
title:                "Kotlin: Berechnen eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnen eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

##Warum

Das Berechnen von zukünftigen oder vergangenen Datumsangaben kann für Entwickler sehr nützlich sein, besonders wenn es um die Planung von Projekten oder Aufgaben geht. Mit Kotlin können Sie dies ganz einfach in wenigen Schritten tun.

##Wie

Zuerst müssen Sie das `Date`-Objekt in Kotlin importieren. Dann können Sie die Funktion `add` verwenden, um eine gewünschte Anzahl von Tagen, Monaten oder Jahren zu einem bestehenden Datum hinzuzufügen oder abzuziehen. Hier ist ein Beispiel, wie Sie 7 Tage zu einem Datum addieren könnten:

```Kotlin
import java.util.Date

fun addDays(date: Date): Date {
    val cal = Calendar.getInstance()
    cal.time = date
    cal.add(Calendar.DAY_OF_MONTH, 7)
    return cal.time
}

// Sample output
val today = Date()
val futureDate = addDays(today)
println(futureDate) // Output: Tue Oct 20 09:54:45 CEST 2020
```

##Tiefes Eintauchen

Sie können auch komplexere Berechnungen durchführen, z.B. das Ermitteln des Datums vor 3 Monaten oder 2 Wochen. Dazu müssen Sie die Funktion `add` mit den entsprechenden Konstanten wie `Calendar.MONTH` oder `Calendar.WEEK_OF_YEAR` verwenden. Hier ist ein Beispiel:

```Kotlin
import java.util.Date

fun subtractMonths(date: Date): Date {
    val cal = Calendar.getInstance()
    cal.time = date
    cal.add(Calendar.MONTH, -3)
    return cal.time
}

// Sample output
val today = Date()
val pastDate = subtractMonths(today)
println(pastDate) // Output: Mon Jul 20 09:54:45 CEST 2020
```

Sie können auch mehr als ein Datum berechnen, indem Sie mehrere Funktionen miteinander kombinieren. Zum Beispiel, um das Datum vor einem Jahr und 3 Monaten zu erhalten, müssten Sie zwei Funktionen kombinieren:

```Kotlin
import java.util.Date

fun subtractMonths(date: Date): Date {
  val cal = Calendar.getInstance()
  cal.time = date
  cal.add(Calendar.MONTH, -3)
  return cal.time
}

fun subtractYears(date: Date): Date {
  val cal = Calendar.getInstance()
  cal.time = date
  cal.add(Calendar.YEAR, -1)
  return cal.time
}

// Sample output
val today = Date()
val pastDate = subtractYears(subtractMonths(today))
println(pastDate) // Output: Mon Jul 20 09:54:45 CEST 2019
```

##Siehe auch

- [Kotlin-Dokumentation zu Datum und Zeit](https://kotlinlang.org/docs/reference/datetime.html)
- [Stack Overflow: Datum in der Zukunft oder Vergangenheit berechnen](https://stackoverflow.com/questions/34038052/get-date-in-the-past-or-future-in-varying-days-in-java)
- [Kotlin Programmieren lernen: Datum und Zeit](https://www.programmierenlernenhq.de/kotlin-datum-und-zeit/)