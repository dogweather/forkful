---
title:    "Kotlin: Das Erhalten des aktuellen Datums"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Warum

Das Abrufen des aktuellen Datums ist ein häufiges Szenario in der Programmierung. Es kann verwendet werden, um das Erstellungsdatum eines Dokuments zu speichern, asynchrone Aufgaben zu planen oder einfach nur das aktuelle Datum in einer Anwendung anzuzeigen.

# Wie

Die aktuelle Datum in Kotlin zu erhalten ist sehr einfach. Dazu können wir die `java.time` API verwenden.

```Kotlin 
// importieren der benötigten Klassen
import java.time.LocalDate
import java.time.format.DateTimeFormatter

// Abrufen des aktuellen Datums als LocalDate-Objekt
val currentDate : LocalDate = LocalDate.now()

// Formatieren des Datums als String mithilfe des DateTimeFormatter-Objekts
val formattedDate : String = currentDate.format(DateTimeFormatter.ofPattern("dd.MM.yyyy"))

// Ausgabe des aktuellen Datums
println("Das aktuelle Datum ist: $formattedDate")
```

Die Ausgabe würde wie folgt aussehen:

```
Das aktuelle Datum ist: 30.08.2021
```

# Deep Dive

Wenn wir uns die `java.time.LocalDate` Klasse genauer ansehen, können wir sehen, dass sie viele nützliche Methoden bietet, um mit Datumswerten umzugehen. Zum Beispiel können wir das Datum um eine beliebige Anzahl von Tagen, Monaten oder Jahren vorwärts oder rückwärts verschieben.

```Kotlin
// Verschieben des aktuellen Datums um 10 Tage in die Zukunft
val futureDate = currentDate.plusDays(10)
println("Das Datum in 10 Tagen ist: ${futureDate.format(DateTimeFormatter.ofPattern("dd.MM.yyyy"))}")
```

Die Ausgabe würde wie folgt aussehen:

```
Das Datum in 10 Tagen ist: 09.09.2021
```

Außerdem bietet die `java.time` API auch Unterstützung für andere Zeitformate wie z.B. Zeitzonen, Uhrzeiten und mehr.

# Siehe auch

- [Kotlin Standard Library - Datum und Uhrzeit](https://kotlinlang.org/docs/tutorials/kotlin-for-py/dates.html)
- [Java API - Local Date Klasse](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java API - Date and Time Tutorial](https://docs.oracle.com/javase/tutorial/datetime/index.html)