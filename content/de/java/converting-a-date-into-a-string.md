---
title:                "Java: Umwandlung eines Datums in einen String"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Warum
Die Umwandlung eines Datums in einen String kann in der täglichen Programmierung sehr nützlich sein, zum Beispiel für die Darstellung von Datumsangaben in einem bestimmten Format oder für die Verwendung in Datenbankabfragen. In diesem Blogbeitrag erklären wir, wie Sie ein Datum in einen String umwandeln können und geben Ihnen Einblicke in die Details dieses Prozesses.

## Wie geht das?
Um ein Datum in einen String umzuwandeln, gibt es verschiedene Ansätze. Ein häufig verwendetes Verfahren ist die Nutzung der SimpleDateFormat-Klasse aus der Java API. Schauen wir uns zunächst ein Beispiel an, wie diese Klasse verwendet werden kann:

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateToStringExample {
    public static void main(String[] args) {
        Date today = new Date();
        SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
        String dateString = formatter.format(today);
        System.out.println("Heute ist der " + dateString);
    }
}
```

In diesem Beispiel erstellen wir eine Instanz der Date-Klasse und initialisieren sie mit dem aktuellen Datum. Anschließend erstellen wir eine Instanz der SimpleDateFormat-Klasse und geben das gewünschte Format als Parameter an. Mit der Methode `format()` können wir nun das Datum in einen String umwandeln und diesen dann ausgeben. Die Ausgabe sollte folgendermaßen aussehen: `Heute ist der 14/08/2021`.

## Tiefere Einblicke
Wenn wir uns den Code näher anschauen, können wir mehr über die Funktionsweise der SimpleDateFormat-Klasse erfahren. Das Format, das wir in unserem Beispiel angegeben haben, besteht aus verschiedenen Buchstaben und Symbolen, die jeweils für eine bestimmte Komponente des Datums stehen. Einige Beispiele sind:

- `dd` für den Tag
- `MM` für den Monat
- `yyyy` für das Jahr
- `HH` für die Stunde
- `mm` für die Minute
- `ss` für die Sekunde

Es gibt noch viele weitere Symbole, die verwendet werden können, um das Datum in verschiedenen Formaten darzustellen. Wenn Sie mehr über diese Symbole und ihre Bedeutung erfahren möchten, können Sie die offizielle Java-Dokumentation zu SimpleDateFormat konsultieren.

Es ist auch wichtig zu beachten, dass die SimpleDateFormat-Klasse nicht thread-safe ist. Wenn Sie also in einer Multithreading-Umgebung arbeiten, sollten Sie eine Instanz pro Thread erstellen oder die Klasse DateFormatUtils aus der Apache Commons libary verwenden, die thread-safe ist.

# Siehe auch
- [Java API-Dokumentation für SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Tutorial zur Verwendung von SimpleDateFormat](https://www.baeldung.com/java-simpledateformat)
- [Dokumentation zu DateFormatUtils aus Apache Commons](https://commons.apache.org/proper/commons-lang/javadocs/api-3.1/org/apache/commons/lang3/time/DateFormatUtils.html)