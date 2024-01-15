---
title:                "Umwandeln eines Datums in einen String"
html_title:           "Java: Umwandeln eines Datums in einen String"
simple_title:         "Umwandeln eines Datums in einen String"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Wenn du jemals eine Datumsangabe in deinem Java-Code benutzt hast, hast du wahrscheinlich schon einmal darüber nachgedacht, wie man ein Datum in einen String umwandelt. Die meisten Anwendungsfälle erfordern die Darstellung des Datums in einem bestimmten Format, sei es für die Ausgabe an den Benutzer oder für die Speicherung in einer Datenbank. Glücklicherweise bietet Java einfache und effiziente Methoden, um dies zu erreichen.

## Wie geht's

Um ein Datum in einen String umzuwandeln, bieten sich die Formatierungsklasse `SimpleDateFormat` und die Methode `format()` an. Hier ist ein Beispiel, das ein aktuelles Datum in den String "27.10.2021" umwandelt:

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

public class DatumZuString {

    public static void main(String[] args) {
        Date jetzt = new Date();
        SimpleDateFormat formatter = new SimpleDateFormat("dd.MM.yyyy");
        String datumAlsString = formatter.format(jetzt);
        System.out.println(datumAlsString);
    }
}
```

Die `SimpleDateFormat`-Klasse akzeptiert ein Muster als Parameter, das die Formatierung des Datums festlegt. Im obigen Beispiel wird das Muster "dd.MM.yyyy" verwendet, um das Datum in Tag.Monat.Jahr zu formatieren. Du kannst das Muster nach deinen Bedürfnissen anpassen, indem du verschiedene Symbole verwendest. Zum Beispiel würde "yyyy-MM-dd" das Datum im Format Jahr-Monat-Tag ausgeben.

## Tiefergehender Einblick

Der grundlegende Gedanke beim Konvertieren eines Datums in einen String ist, dass das Datum als eine Nummer gespeichert wird, die die Anzahl der Millisekunden seit dem 1. Januar 1970 (auch bekannt als Unix-Zeit) darstellt. Indem du ein geeignetes Formatmuster verwendest, kannst du diese Nummer in das gewünschte Datum umwandeln.

Einige nützliche Symbole für das Formatmuster sind:

- `y`: Jahr in voller Länge (z.B. 2021)
- `M`: Monat als Zahl (z.B. 10 für Oktober)
- `MM`: Monat als zweistellige Zahl (z.B. 10 für Oktober)
- `MMM`: Monatsname abgekürzt (z.B. Okt für Oktober)
- `MMMM`: Monatsname in voller Länge (z.B. Oktober)
- `dd`: Tag als zweistellige Zahl (z.B. 27)
- `EEE`: Wochentag abgekürzt (z.B. Mi für Mittwoch)
- `EEEE`: Wochentag in voller Länge (z.B. Mittwoch)
- `HH`: Stunde im 24-Stunden-Format (z.B. 17 für 5 Uhr nachmittags)
- `mm`: Minute (z.B. 34)
- `ss`: Sekunde (z.B. 21)

Es gibt viele weitere Symbole und Kombinationen, die du verwenden kannst. Eine vollständige Liste findest du in der offiziellen [Java-Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html).

## Siehe auch

- [Offizielle Java-Dokumentation für SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Tutorial zum Konvertieren von Dateien in Java](https://www.tutorialspoint.com/java/java_date_time.htm)
- [Gebräuchliche Muster für die Datumsformatierung](https://www.w3.org/standards/techs/date-basic)