---
title:                "Ein Datum aus einem String extrahieren"
html_title:           "Java: Ein Datum aus einem String extrahieren"
simple_title:         "Ein Datum aus einem String extrahieren"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen eines Datums aus einem String ist ein häufiger Vorgang in der Programmierung, bei dem ein Datum, das in einem lesbaren Textformat vorliegt, in ein von der Software verarbeitbares Format umgewandelt wird. Programmierer tun dies, um beispielsweise Benutzereingaben, Datenbankabfragen oder API-Antworten zu verarbeiten.

## Wie geht's?

Das Parsen eines Datums aus einem String in Java ist mit der Klasse "SimpleDateFormat" und einem bestimmten Datumsformat-String recht einfach. Schauen wir uns ein Beispiel an:

```Java
// Erstelle eine neue Instanz von SimpleDateFormat
SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy");
// Lege einen String mit dem Datum fest, das geparst werden soll
String dateAsString = "08/03/2020";
// Verwende die parse() Methode, um den String in ein Datum umzuwandeln
Date parsedDate = sdf.parse(dateAsString);
// Gib das Format des geparsten Datums aus
System.out.println("Geparstes Datum: " + parsedDate);
```

Die Ausgabe dieses Codes wird sein: "Geparstes Datum: Sun Mar 08 00:00:00 GMT 2020". Somit wurde das Datum erfolgreich von einem lesbaren String in ein von der Software verarbeitbares Format umgewandelt.

## Tiefergehende Informationen

Das Parsen von Daten aus einem String ist ein wichtiger Teil der Programmierung und wird seit vielen Jahren verwendet. Früher war die Klasse "SimpleDateFormat" die einzige Möglichkeit, Datumsformatierungen in Java durchzuführen. Mittlerweile gibt es aber auch die neuen Klassen "LocalDate" und "DateTimeFormatter", die fortschrittlichere Funktionen bieten.

Eine Alternative zum Parsen von Daten aus einem String ist die Verwendung von Regulären Ausdrücken in Verbindung mit der "Pattern" Klasse, um das gewünschte Datumsmuster zu identifizieren und zu extrahieren. Dies kann in manchen Fällen präziser sein als die Verwendung von "SimpleDateFormat".

Bei der Implementierung des Parsens eines Datums aus einem String ist es wichtig, auf die richtige Verwendung von Formatierungssymbolen zu achten, da dies zu Fehlern führen kann, wenn sie falsch eingesetzt werden. Eine ausführliche Dokumentation zu den Formatierungssymbolen von "SimpleDateFormat" ist auf der offiziellen Java-Dokumentationsseite zu finden.

## Siehe auch

- [Oracle Java-Dokumentation zum Parsen von Datumsangaben](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Tutorial zum Parsen von Datumsangaben in Java](https://www.baeldung.com/java-string-to-date)