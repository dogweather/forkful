---
title:                "Ein neues Projekt beginnen"
html_title:           "Arduino: Ein neues Projekt beginnen"
simple_title:         "Ein neues Projekt beginnen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Starten eines neuen Projekts bedeutet, ein neues Programm für dein Arduino-Board zu schreiben und auszuführen. Programmierer machen das, um ihre Ideen zum Leben zu erwecken und ihre Fähigkeiten zu verbessern.

## Wie geht's:
Ein Beispiel, um ein einfaches "Hallo, Welt!" Programm zu erstellen:
```Arduino
void setup(){
  Serial.begin(9600);
}

void loop(){
  Serial.println("Hallo, Welt!");
  delay(1000);
}
```
Ausgabe:
```
Hallo, Welt!
Hallo, Welt!
Hallo, Welt!
...
```

## Tiefgehende Informationen:
(1) Die Entwicklung von Arduino begann im Jahr 2005 als ein Projekt von Massimo Banzi und David Cuartielles, um Studenten den Einstieg in die Mikrocontroller-Programmierung zu erleichtern. (2) Es gibt auch Alternativen zu Arduino, wie z.B. Raspberry Pi, die ebenfalls als Plattform für Embedded-Systeme verwendet werden können. (3) Um ein neues Projekt zu starten, musst du zunächst ein Board auswählen, die erforderlichen Komponenten anschließen und dann dein Programm mithilfe der Arduino-IDE hochladen.

## Siehe auch:
Weitere Informationen zum Erstellen von Projekten mit Arduino findest du auf der offiziellen Website: https://www.arduino.cc/en/Guide/HomePage