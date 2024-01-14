---
title:    "C: Das Berechnen eines Datums in der Zukunft oder Vergangenheit"
keywords: ["C"]
---

{{< edit_this_page >}}

## Warum
Die Berechnung eines Datums in der Zukunft oder Vergangenheit kann in vielen Fällen nützlich sein, wie zum Beispiel bei der Planung von Terminen oder der Verarbeitung von Datumseingaben in einem Programm.

## Wie geht das
Die Berechnung eines Datums in der Zukunft oder Vergangenheit erfordert einige grundlegende Schritte, die wir uns in diesem Artikel genauer ansehen werden.

Zuerst müssen wir das aktuelle Datum als Ausgangspunkt nehmen. Dies kann mit der Funktion `time()` erreicht werden, die die Anzahl der Sekunden seit dem 1. Januar 1970 zurückgibt.

```C
time_t now = time(NULL);
```

Als nächstes müssen wir entscheiden, ob wir das Datum in der Zukunft oder Vergangenheit berechnen wollen, und wie viele Tage wir dazuzählen oder abziehen möchten. Dies kann durch einfache mathematische Berechnungen mit der Anzahl der Sekunden pro Tag erfolgen.

```C
// Berechnung für ein zukünftiges Datum
time_t future = now + (numberOfDays * 86400);

// Berechnung für ein vergangenes Datum
time_t past = now - (numberOfDays * 86400);
```

Schließlich müssen wir das berechnete Datum in ein für den Benutzer lesbares Format umwandeln. Hier können wir die Funktion `localtime()` verwenden, um eine Struktur mit allen Informationen über das Datum zu erhalten.

```C
// Berechne UTC Date/Time Struktur aus 'future' Timestamp
struct tm *timeinfo = localtime(&future);
```

Wenn wir nun `timeinfo` ausgeben, erhalten wir alle Informationen über das zukünftige Datum, einschließlich Jahr, Monat, Tag usw.

```C
printf("Das berechnete Datum ist: %s", asctime(timeinfo));
```

## Tiefergehende Informationen
Die Berechnung eines Datums in der Zukunft oder Vergangenheit kann komplexer sein, abhängig von den Anforderungen eines spezifischen Programms. Zum Beispiel muss man möglicherweise auch die Schaltjahre oder unterschiedliche Anzahlen von Tagen in verschiedenen Monaten berücksichtigen. Es gibt auch viele hilfreiche Funktionen und Bibliotheken, die bei der Berechnung von Datumswerten helfen können, wie beispielsweise `mktime()` oder die `time.h` Bibliothek.

Siehe auch
- [Berechnung eines Datums im C-Programm](https://www.thoughtco.com/c-programming-calculating-a-date-958369)
- [Dokumentation zur `time.h` Bibliothek](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Verwendung von `mktime()` in C](https://www.geeksforgeeks.org/mktime-in-c/)