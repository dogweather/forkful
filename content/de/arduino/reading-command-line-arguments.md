---
title:                "Befehlszeilenargumente lesen"
html_title:           "Arduino: Befehlszeilenargumente lesen"
simple_title:         "Befehlszeilenargumente lesen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Was & Warum?
Command Line Arguments sind Eingaben, die Sie beim Aufruf eines Programms in der Befehlszeile hinzufügen. Sie ermöglichen es Programmierern, Programmen spezifische Ausführungsparameter zu geben, was die Anpassungsfähigkeit und Effizienz erhöht.

# Wie geht das:
Arduino-Code akzeptiert keine Befehlszeilenargumente direkt, aber wir können eine ähnliche Funktionalität mit `Serial` Klassenmethoden simulieren. Unten ist ein einfacher Code:

```Arduino
String input;

void setup(){
  Serial.begin(9600);
}

void loop(){
  while(Serial.available()){
      char inChar = (char)Serial.read();
      input += inChar;
  }

  if(input.length() > 0){
    Serial.println("Sie haben eingegeben: " +  input);
    input = "";
  }
}
```
Schreiben Sie etwas im Serial Monitor und sehen Sie die Ausgabe als Echo des eingegebenen Textes.

# Tiefgehende Erklärung
Historisch gesehen war die Verwendung von Command Line Arguments in den frühen Zeiten der Datenverarbeitung weit verbreitet. Bei älteren Systemen wie CP/M und MS-DOS war dies eine grundlegende Technik zur Interaktion mit Programmen.

In Arduino sind die Dinge ein bisschen anders. Aufgrund der Natur von Mikrokontrollern akzeptiert Arduino keine Befehlszeilenargumente auf herkömmliche Weise. Aber die Serial-Klasse und ihre Methoden können dazu genutzt werden, um eine vergleichbare Funktionalität bereitzustellen.

Alternative Ansätze könnten beinhalten, einen Parser zu implementieren, der CSV- oder JSON-formatierte Daten akzeptiert, um komplexere Konfigurationen zu handhaben.

# Siehe auch
Die Dokumentation zur `Serial` Klasse finden Sie [hier](https://www.arduino.cc/reference/de/language/functions/communication/serial/).

Für eine detailliertere Erklärung von Command Line Arguments, schauen Sie sich diesen [Link](https://de.wikipedia.org/wiki/Parameter_(Informatik)) an.