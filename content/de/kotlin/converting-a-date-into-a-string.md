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

## Warum

Das Konvertieren eines Datums in einen String kann hilfreich sein, wenn du in deiner Kotlin-Anwendung Daten speicherst oder sie daraus ausliest. Beispielsweise könntest du ein Datum als String speichern, um es später in einem Textfeld anzuzeigen oder es in eine Datenbank zu übertragen.

## Wie geht das?

Die Konvertierung eines Datums in einen String in Kotlin ist sehr einfach. Du kannst die `SimpleDateFormat`-Klasse verwenden, um das Format des Datums anzugeben und es in einen String umzuwandeln. Hier ist ein Beispiel für ein Datum, das in das Format "TT.MM.JJJJ" umgewandelt wird:

```Kotlin
val datum = Date()
val sdf = SimpleDateFormat("dd.MM.yyyy")
val stringDatum = sdf.format(datum)
println(stringDatum) // gibt z.B. "04.03.2021" aus
```

Du kannst auch eine eigene Methode erstellen, um dieses Beispiel zu vereinfachen:

```Kotlin
fun konvertiereDatumZuString(datum: Date, format: String): String {
    val sdf = SimpleDateFormat(format)
    return sdf.format(datum)
}
```

Und dann die Methode mit den gewünschten Parametern aufrufen:

```Kotlin
val datum = Date()
val stringDatum = konvertiereDatumZuString(datum, "dd.MM.yyyy")
println(stringDatum) // gibt z.B. "04.03.2021" aus
```

## Tiefergehende Einblicke

Die `SimpleDateFormat`-Klasse bietet viele Optionen, um das Ausgabeformat des Datums anzupassen. Du kannst z.B. auch die Wochentage mit ausgeben lassen oder die Monatsnamen in verschiedenen Sprachen darstellen.

Außerdem empfiehlt es sich, das Konvertieren von Datum zu String in einer separaten Hilfsmethode zu implementieren, um Code-Duplikation zu vermeiden und die Lesbarkeit zu verbessern.

## Siehe auch

- [Java SimpleDateFormat Documentation](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Kotlin Dates and Times](https://kotlinlang.org/docs/dates-times.html)