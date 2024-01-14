---
title:                "Kotlin: Umwandlung eines Datums in einen String"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum?

Das Konvertieren von Datum zu String ist ein grundlegender Schritt in der Datumsmanipulation. Durch die Umwandlung eines Datums in einen String können Daten in einem für den Benutzer lesbaren Format angezeigt werden. Außerdem ermöglicht es die Verwendung von Datumswerten in anderen Datentypen wie beispielsweise in URL-Parametern oder in Textfeldern.

## Wie geht's?

Um ein Datum in einen String umzuwandeln, kann die Funktion `format()` der Klasse `SimpleDateFormat` verwendet werden. Diese Funktion nimmt als Parameter ein Datumsmuster und gibt den entsprechenden String zurück. Hier ist ein Beispiel dafür, wie ein Datum in ein deutsches Format konvertiert werden kann:

```Kotlin
val date = Date()
val df = SimpleDateFormat("dd.MM.yyyy")

val formattedDate = df.format(date)
println(formattedDate) // Output: 14.07.2021
```

Es ist wichtig zu beachten, dass bei der Verwendung von `SimpleDateFormat` die Spracheinstellungen des Systems berücksichtigt werden. Wenn beispielsweise die Sprache auf Deutsch eingestellt ist, wird das Datum automatisch in deutschem Format ausgegeben.

Eine weitere Möglichkeit, ein Datum in einen String umzuwandeln, ist die Verwendung von `DateTimeFormatter` aus der `java.time` Bibliothek. Hier ist ein Beispiel dafür, wie ein Datum in ein benutzerdefiniertes Format konvertiert werden kann:

```Kotlin
val date = LocalDateTime.now()
val formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss")

val formattedDate = date.format(formatter)
println(formattedDate) // Output: 14.07.2021 14:45:32
```

Es gibt auch die Möglichkeit, mithilfe der `toString()` Methode des `Date` Objekts ein Datum in einen String zu konvertieren. Diese Methode gibt das Datum in einem standardisierten Format zurück, das je nach Spracheinstellungen variieren kann.

## Tiefere Einblicke

Beim Konvertieren von Datum zu String ist es wichtig, das richtige Muster zu wählen. Es gibt verschiedene Muster für das Datum, die in der Klasse `SimpleDateFormat` verwendet werden können, z.B. "dd.MM.yyyy" für das deutsche Format oder "MM/dd/yyyy" für das amerikanische Format. Eine vollständige Liste der verfügbaren Muster findet man in der offiziellen [Dokumentation](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html).

Außerdem ist es wichtig zu beachten, dass das Umwandeln von Datum zu String auch umgekehrt möglich ist. Das bedeutet, dass ein String in ein Datum konvertiert werden kann, um es weiter zu verwenden. In diesem Fall muss jedoch auch das entsprechende Muster verwendet werden und der String muss in ein gültiges Datum umgewandelt werden können.

In Kotlin steht außerdem die `Date` Klasse zur Verfügung, die verschiedene Methoden zur Datumsmanipulation beinhaltet. Diese können bei Bedarf in Kombination mit der Umwandlung von Datum zu String verwendet werden.

## Siehe auch

- [Kotlin-Dokumentation zur `SimpleDateFormat` Klasse](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-simple-date-format/)
- [Offizielle Oracle-Dokumentation zu `SimpleDateFormat`](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- [Kotlin-Dokumentation zur `DateTimeFormatter` Klasse](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-date-time-formatter/)
- [Offizielle Oracle-Dokumentation zu `DateTimeFormatter`](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)