---
title:                "Java: Ein Datum in eine Zeichenkette umwandeln"
simple_title:         "Ein Datum in eine Zeichenkette umwandeln"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Die Umwandlung von einem Datum in einen String ist eine häufige Aufgabe in Java-Programmierung. Mit dieser Fähigkeit können Sie beispielsweise die Darstellung von Datums- und Zeitangaben in Ihrem Code anpassen oder in einem bestimmten Format speichern. In diesem Blog-Beitrag werde ich Ihnen zeigen, wie Sie dieses Problem lösen können.

## How To

Bevor wir uns in die Details stürzen, werfen wir einen Blick auf ein einfaches Beispiel:

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateToStringConverter {

    public static void main(String[] args) {
        // Erstelle ein Date-Objekt mit dem aktuellen Datum
        Date now = new Date();
        
        // Definiere das gewünschte Datumsformat
        SimpleDateFormat dateFormat = new SimpleDateFormat("dd.MM.yyyy");
        
        // Wandele das Datum in einen String um
        String dateAsString = dateFormat.format(now);
        
        // Gib den String aus
        System.out.println("Datum als String: " + dateAsString);
    }
}
```

**Output:**

```
Datum als String: 23.06.2021
```

In diesem Beispiel haben wir das aktuelle Datum in das Format "dd.MM.yyyy" umgewandelt, also Tag.Monat.Jahr. Aber wie funktioniert das genau? 

Zunächst haben wir ein Date-Objekt mit dem aktuellen Datum erstellt. Dann haben wir ein SimpleDateFormat-Objekt erstellt, das uns erlaubt, das Datum in das gewünschte Format umzuwandeln. Die verwendete Syntax folgt bestimmten Buchstabenmuster, die im [Java-Dokumentationshandbuch](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html) vollständig erklärt werden. Schließlich haben wir die format()-Methode aufgerufen und das Datum als String gespeichert.

## Deep Dive

Nun, da wir wissen, wie wir ein Datum in einen String umwandeln können, betrachten wir einige wichtige Punkte, die man bei der Verwendung dieser Methode beachten sollte:

- Das Date-Objekt beinhaltet neben dem Datum auch die Uhrzeit. Wenn Sie nur das Datum in einen String umwandeln möchten, müssen Sie das Date-Objekt vorher in ein Calendar-Objekt konvertieren.
- Wenn Sie ein bestimmtes Datumsformat benötigen, das nicht in der Dokumentation aufgeführt ist, können Sie auch eigene Muster definieren. Beachten Sie jedoch, dass hierfür einige Syntaxregeln zu beachten sind.
- Wenn Sie das Datum in einem anderen Zeitzonenumwandeln möchten, können Sie die TimeZone-Klasse verwenden.

## See Also

- [Java-Dokumentationshandbuch für SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- [Tutorial for Java SimpleDateFormat](https://www.tutorialspoint.com/java/java_date_time.htm)
- [Eigene Muster für SimpleDateFormat erstellen](https://www.baeldung.com/java-date-format)

Vielen Dank, dass Sie diesen Blog-Beitrag gelesen haben. Ich hoffe, er hat Ihnen geholfen, die Konvertierung von Datum zu String besser zu verstehen und in Ihren zukünftigen Java-Projekten anzuwenden. Happy coding!