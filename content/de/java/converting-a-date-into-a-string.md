---
title:    "Java: Umwandlung eines Datums in einen String"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Warum
Das Konvertieren von einem Datum in einen String ist ein grundlegender Schritt in der Java Programmierung, der für die korrekte Darstellung von Datumswerten in verschiedenen Formaten unverzichtbar ist.

## Wie geht das?
Die Konvertierung erfordert die Verwendung der `SimpleDateFormat` Klasse, die es uns ermöglicht, ein gewünschtes Datumsformat anzugeben. Im Folgenden ist ein Beispielcode dargestellt, der ein Datum in den String "dd.MM.yyyy" (Tag.Monat.Jahr) konvertiert:

```Java
SimpleDateFormat format = new SimpleDateFormat("dd.MM.yyyy");
Date date = new Date();
String dateString = format.format(date);
System.out.println(dateString);
```

Die Ausgabe dieses Codes wäre zum Beispiel "07.03.2021".

## Tiefergehende Informationen
Bei der Konvertierung von einem Datum in einen String gibt es einige wichtige Aspekte zu beachten. Zum Beispiel können bestimmte Buchstaben in der `SimpleDateFormat` Klasse verschiedene Bedeutungen haben, abhängig von der verwendeten Locale (Länder- oder Spracheinstellung). Es ist daher wichtig, das gewünschte Format sorgfältig zu prüfen, um unerwartete Ergebnisse zu vermeiden.

Ein weiterer wichtiger Punkt ist, dass die `SimpleDateFormat` Klasse nicht threadsicher ist. Daher sollten Entwickler sicherstellen, dass mehrere Threads nicht gleichzeitig darauf zugreifen, da dies zu unerwarteten Fehlern führen kann.

## Siehe auch
- [Java Date and Time API documentation](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Tutorial zu SimpleDateFormatter in Java](https://www.baeldung.com/java-simpledateformat)
- [Java-Tipp: Erkenntnisse über Thread-Sicherheit](https://www.onjava.com/pub/a/onjava/2003/12/17/threads.html)