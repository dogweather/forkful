---
title:    "Kotlin: Vergleich von zwei Daten"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Daten kann ein wichtiger Teil jedes Programms sein, das mit Zeitangaben und Terminen zu tun hat. Es ermöglicht uns, zu überprüfen, ob eine bestimmte Zeit bereits vergangen ist oder in der Zukunft liegt und gibt uns die Möglichkeit, entsprechend zu handeln.

## Wie geht das?

In Kotlin gibt es mehrere Möglichkeiten, um zwei Datumswerte zu vergleichen. Die einfachste Methode ist die Verwendung von Operatoren wie `<`, `>` oder `==`. Diese ermöglichen es uns, die Reihenfolge der Daten zu vergleichen oder sie auf Gleichheit zu überprüfen.

```Kotlin
val date1 = LocalDate.now()
val date2 = LocalDate(2020,1,1)

if (date1 > date2) {
   println("date1 liegt nach date2")
}

if (date1 == date2) {
   println("date1 und date2 sind gleich")
}
```

Eine weitere Möglichkeit ist die Verwendung von `isAfter()` und `isBefore()` Methoden, die speziell für die Klasse `LocalDate` von Kotlin erstellt wurden.

```Kotlin
val date1 = LocalDate.now()
val date2 = LocalDate(2022,1,1)

if (date1.isAfter(date2)) {
   println("date1 liegt nach date2")
}

if (date1.isBefore(date2)) {
   println("date1 liegt vor date2")
}
```

Es ist auch möglich, Datumswerte in Millisekunden zu vergleichen, indem man sie in einen Zeitstempel umwandelt und dann den Vergleich durchführt.

```Kotlin
val date1 = LocalDate.now()
val date2 = LocalDate(2025,1,1)

val ts1 = date1.toEpochSecond()
val ts2 = date2.toEpochSecond()

if (ts1 < ts2) {
   println("date1 liegt vor date2")
}
```

## Tiefer ins Detail

Bei der Verwendung der oben genannten Methoden ist es wichtig zu beachten, dass der Vergleich abhängig von der aktuellen Zeitzone erfolgt. Daher können je nach Ort Unterschiede bei den Ergebnissen auftreten.

Um diese Probleme zu vermeiden, ist es empfehlenswert, Datumsobjekte in UTC (koordinierte Weltzeit) umzuwandeln und dann den Vergleich durchzuführen.

```Kotlin
val date1 = LocalDate.now()
val date2 = LocalDate(2028,1,1)

val date1Utc = date1.atStartOfDay(ZoneOffset.UTC).toInstant()
val date2Utc = date2.atStartOfDay(ZoneOffset.UTC).toInstant()

if (date1Utc.isBefore(date2Utc)) {
   println("date1 liegt vor date2")
}
```

## Siehe auch

- [Offizielle Dokumentation von Kotlin über Datum und Zeit](https://kotlinlang.org/docs/datetime.html)
- [StackOverflow Frage zum Vergleichen von Datumswerten in Kotlin](https://stackoverflow.com/questions/48342043/compare-between-two-localdate-in-java-8)
- [Medium-Artikel über den Umgang mit Datumswerten in Kotlin](https://medium.com/@kenkyee/working-with-dates-using-kotlin-2feb24c72d3a)