---
title:    "Kotlin: Eine Datum in einen String umwandeln."
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Warum

Es gibt viele Gründe, warum man ein Datum in eine Zeichenfolge konvertieren möchte. Zum Beispiel kann man so ein Datum leichter als Teil einer größeren Zeichenfolge verwenden oder es auf eine bestimmte Art und Weise formatieren, um es für bestimmte Zwecke zu nutzen.

# Anleitung

Die Konvertierung eines Datums in eine Zeichenfolge ist in Kotlin sehr einfach. Zunächst muss das Datum in das entsprechende Datumsobjekt umgewandelt werden. Dann kann man die Funktion `format()` verwenden, um das Datum in eine Zeichenfolge mit dem gewünschten Format umzuwandeln. Hier ist ein Beispielcode:

```Kotlin
val date = Date()
val dateFormat = SimpleDateFormat("dd.MM.yyyy")
val dateString = dateFormat.format(date)
println(dateString)
```

Der Output wäre dann in diesem Fall beispielsweise "29.09.2021".

# Tiefergehende Informationen

In Kotlin gibt es verschiedene Möglichkeiten, ein Datum in eine Zeichenfolge umzuwandeln. Man kann zum Beispiel auch die Funktion `toString()` verwenden, die standardmäßig das Datum im ISO-8601-Format zurückgibt. Außerdem gibt es zahlreiche verschiedene Formate, die man für die Funktion `format()` verwenden kann. Diese können zum Beispiel Monatsnamen oder Wochentage enthalten.

Es ist auch wichtig zu beachten, dass bei der Konvertierung eines Datums in eine Zeichenfolge die aktuelle Zeitzone des Geräts berücksichtigt wird. Um sicherzustellen, dass das richtige Datum ausgegeben wird, sollte man die Zeitzone manuell festlegen oder auf UTC umstellen.

# Siehe auch

- [Dokumentation zur Funktion `format()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-java-date/format.html)
- [Ein Tutorial zur Datumsformatierung in Kotlin](https://www.baeldung.com/kotlin-datetime-format)
- [Weitere Informationen über das Datumsobjekt in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date/)