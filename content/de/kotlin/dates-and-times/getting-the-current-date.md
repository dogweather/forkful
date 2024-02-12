---
title:                "Den aktuellen Datum abrufen"
aliases: - /de/kotlin/getting-the-current-date.md
date:                  2024-02-03T19:10:16.655169-07:00
model:                 gpt-4-0125-preview
simple_title:         "Den aktuellen Datum abrufen"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
In der Programmierung ist das Abrufen des aktuellen Datums eine grundlegende Aufgabe, die es Entwicklern ermöglicht, auf das aktuelle Datum zuzugreifen, es anzuzeigen oder damit zu arbeiten in ihren Anwendungen. Diese Fähigkeit ist entscheidend für alles, von der Protokollierung und Zeitstempelung von Ereignissen bis hin zu Berechnungen basierend auf Daten.

## Wie geht das:

### Verwendung von Standard Kotlin
Kotlin hat keine eigene Datum- und Zeit-API, sondern verlässt sich auf die Java Standardbibliothek für diese Funktionalität. So können Sie das aktuelle Datum abrufen:

```kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("Heutiges Datum: $today")
}
```

**Beispielausgabe:**
```
Heutiges Datum: 2023-04-05
```

### Verwendung von java.util.Date
Für Operationen, die sowohl das Datum als auch die Zeit erfordern, könnte `java.util.Date` bevorzugt werden.

```kotlin
import java.util.Date

fun main() {
    val currentDate = Date()
    println("Aktuelles Datum und Uhrzeit: $currentDate")
}
```

**Beispielausgabe:**
```
Aktuelles Datum und Uhrzeit: Mi Apr 05 15:20:45 GMT 2023
```

### Verwendung der Joda-Time Bibliothek
Bevor Java 8 eine neue Datum- und Zeit-API einführte, war Joda-Time der de-facto Standard für Datum-Zeit-Operationen in Java und Kotlin. Auch wenn es für viele Projekte nicht mehr notwendig ist, verwenden einige es möglicherweise immer noch aus Gründen der Abwärtskompatibilität oder persönlichen Vorliebe.

Fügen Sie die Joda-Time-Bibliothek zur build.gradle-Datei Ihres Projekts hinzu:
```
implementation 'joda-time:joda-time:2.10.10'
```

```kotlin
import org.joda.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("Heutiges Datum: $today")
}
```

**Beispielausgabe:**
```
Heutiges Datum: 2023-04-05
```

### Verwendung von ThreeTenABP für Android
Für die Android-Entwicklung wird die Verwendung des Backports der Java Time API über das ThreeTen Android Backport Project für Versionen vor Android API Level 26 empfohlen.

Fügen Sie die Abhängigkeit zur build.gradle-Datei Ihrer App hinzu:
```
implementation 'com.jakewharton.threetenabp:threetenabp:1.3.1'
```

Initialisieren Sie es in Ihrer Application-Klasse:
```kotlin
import android.app.Application
import com.jakewharton.threetenabp.AndroidThreeTen

class MyApp : Application() {
    override fun onCreate() {
        super.onCreate()
        AndroidThreeTen.init(this)
    }
}
```

Dann können Sie es wie folgt verwenden:
```kotlin
import org.threeten.bp.LocalDate

fun main() {
    val today = LocalDate.now()
    println("Heutiges Datum: $today")
}
```

**Beispielausgabe:**
```
Heutiges Datum: 2023-04-05
```
