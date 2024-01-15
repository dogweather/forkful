---
title:                "Arbeiten mit CSV"
html_title:           "Arduino: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man CSV-Daten mit Arduino verarbeiten? Nun, CSV (Comma Separated Values) ist ein gängiges Dateiformat für strukturierte Daten. Es wird häufig verwendet, um Daten zwischen verschiedenen Programmen auszutauschen und ist daher auch nützlich für die Kommunikation zwischen einem Arduino und einem Computer.

## Wie es geht

Um CSV-Daten mit Arduino zu verarbeiten, müssen wir die standardmäßige Arduino-CSV-Bibliothek verwenden. Hier ist ein Beispielcode, der eine CSV-Datei von einem Computer liest und die Werte auf einem seriellen Monitor ausgibt:

```Arduino
#include <CSV.h>

void setup() {
  Serial.begin(9600); // Startet die Kommunikation mit dem seriellen Monitor
}

void loop() {
  // Öffne eine Verbindung zum Computer durch die Verwendung des standardmäßigen seriellen Anschlusses
  File dataFile = Serial.open("data.csv", FILE_READ); 

  // Erstelle eine CSV-Instanz mit der Verbindung zu der geöffneten Datei
  CSV csv = CSV(dataFile);

  // Lies den Inhalt der Datei Zeile für Zeile ein
  int rowNum = 0;
  while (csv.available()) {

    // Holt die nächste Zeile als String
    String csvData = csv.readString();

    // Splittet die Zeile in einzelne Werte auf
    String values[4];
    csvData.split(values, ",");

    // Gibt die entsprechenden Werte auf dem seriellen Monitor aus
    Serial.println("Werte der " + String(rowNum+1) + ".Zeile:");
    Serial.println("Wert 1: " + values[0]);
    Serial.println("Wert 2: " + values[1]);
    Serial.println("Wert 3: " + values[2]);
    Serial.println("Wert 4: " + values[3]);
    
    rowNum++;
  }
}
```

Die Ausgabe auf dem seriellen Monitor wird wie folgt aussehen:

```
Werte der 1.Zeile:
Wert 1: 42
Wert 2: 3.14
Wert 3: "Arduino"
Wert 4: "CSV"
Werte der 2.Zeile:
Wert 1: 15
Wert 2: 2.71
Wert 3: "Hallo"
Wert 4: "Welt"
```

Beachte, dass jede Zeile in der CSV-Datei aus vier Werten besteht, die durch ein Komma getrennt sind. Die `split()` Funktion teilt diese Werte basierend auf dem Komma und speichert sie in einem Array namens `values`.

## Tiefer eintauchen

Wenn du tiefer in die Verarbeitung von CSV-Daten mit Arduino eintauchen möchtest, gibt es einige wichtige Funktionen der CSV-Bibliothek, die du kennen solltest:

- `open()` öffnet eine Verbindung zu einer CSV-Datei auf einem externen Speichergerät wie einer SD-Karte oder einem USB-Stick.
- `available()` gibt `true` zurück, wenn noch Daten zum Lesen in der Datei vorhanden sind.
- `readString()` liest die nächste Zeile der Datei als String ein.
- `split()` teilt einen String anhand eines Trennzeichens und speichert die Werte in einem Array.
- `getString()` gibt den Wert in einer bestimmten Spalte zurück.
- `getInt()` gibt den Wert in einer bestimmten Spalte als Integer zurück.
- `getFloat()` gibt den Wert in einer bestimmten Spalte als Gleitkommazahl zurück.

Ein weiterer wichtiger Punkt ist die Verwendung von CSV-Formatierung beim Schreiben von Daten. Im Allgemeinen sollten Zeichen wie Kommas, Anführungszeichen oder Newlines immer in Anführungszeichen eingeschlossen werden, um sicherzustellen, dass sie korrekt interpretiert werden. Ein Beispiel dafür wäre `"Hallo, Welt"`. Wenn in einem Wert bereits Anführungszeichen enthalten sind, sollten diese durch ein Backslash (`\`) escapet werden, z.B. `" \" Guten Tag \" "`.

Um mehr über die CSV-Bibliothek zu erfahren, kannst du die offizielle Dokumentation [hier] (https://github.com/rodan/arduino-CSV) lesen.

## Siehe auch

- [Arduino Forumsthread] (https://forum.arduino.cc/index.php?topic=399239.msg2741314#msg2741314)
- [