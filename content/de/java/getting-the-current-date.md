---
title:                "Das aktuelle Datum abrufen"
html_title:           "Java: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Erhalten des aktuellen Datums ist eine häufige Aufgabe in der Programmierung. Es ermöglicht die Verarbeitung von Daten in Echtzeit und erleichtert die Organisation von Dateien und Ordnern.

## Wie geht's?
Um das aktuelle Datum in Java zu erhalten, verwendet man die ```LocalDate``` Klasse aus dem ```java.time``` Paket. Hier ist ein Beispielcode:

```Java
import java.time.LocalDate;

public class CurrentDateExample {

    public static void main(String[] args) {
        
        // Erstellt ein LocalDate Objekt mit dem heutigen Datum
        LocalDate today = LocalDate.now();
        
        // Gibt das aktuelle Datum aus
        System.out.println("Heute ist der " + today);
    }
}
```

Die Ausgabe dieses Codes wäre:

```shell
Heute ist der 2021-01-01
```

Man kann auch das aktuelle Datum im gewünschten Format ausgeben lassen, z.B. in deutschem Format mit dem Tag zuerst und dem Monat ausgeschrieben:

```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class CurrentDateExample {

    public static void main(String[] args) {
        
        // Erstellt ein LocalDate Objekt mit dem heutigen Datum
        LocalDate today = LocalDate.now();
        
        // Erstellt ein DateTimeFormatter Objekt mit dem gewünschten Format
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy");
        
        // Gibt das aktuelle Datum im gewünschten Format aus
        System.out.println("Heute ist der " + today.format(formatter));
    }
}
```

Die Ausgabe wäre dann:

```shell
Heute ist der 01.01.2021
```

## Tiefere Einblicke
Das Erhalten des aktuellen Datums ist in Java seit der Einführung des ```java.time``` Pakets in Java 8 viel einfacher geworden. Vorher musste man dafür die ```Calendar``` Klasse verwenden, was oft komplizierter war.

Eine Alternative zur Nutzung der ```LocalDate``` Klasse ist die Verwendung der ```Date``` Klasse, aber diese wird nun als veraltet (deprecated) betrachtet und sollte vermieden werden, da sie Schwierigkeiten beim Umgang mit Zeitzonen und Formatierung verursachen kann.

## Siehe auch
- [Java Oracle Dokumentation zu LocalDate](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html)
- [Tutorialspoint Artikel zu Java Dates](https://www.tutorialspoint.com/java/java_date_time.htm)