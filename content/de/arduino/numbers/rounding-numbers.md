---
title:                "Zahlen runden"
aliases:
- /de/arduino/rounding-numbers.md
date:                  2024-01-26T03:42:33.761940-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zahlen runden"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/rounding-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Runden von Zahlen ist das Trimmen einer Dezimalzahl auf den nächstgelegenen ganzen Wert oder auf eine festgelegte Anzahl von Dezimalstellen. Programmierer runden Zahlen, um sie leichter lesbar und handhabbar zu machen, besonders wenn Präzision über einen bestimmten Punkt hinaus unnötig ist oder zu Fehlern führen könnte.

## Wie geht das:
In Arduino können Sie Zahlen mit eingebauten Funktionen runden. Die Hauptakteure sind `round`, `ceil` und `floor`. Hier ist eine schnelle Demo:

```arduino
void setup() {
  Serial.begin(9600);
  
  float myNumber = 123.4567;

  // Auf die nächste ganze Zahl runden
  Serial.println(round(myNumber)); // Gibt aus: 123

  // Rundet immer auf
  Serial.println(ceil(myNumber));  // Gibt aus: 124

  // Rundet immer ab
  Serial.println(floor(myNumber)); // Gibt aus: 123
}

void loop() {
  // Nichts zum Durchlaufen.
}
```

## Tiefergehend:
Rundungsalgorithmen haben eine lange Geschichte; sie gibt es schon lange vor den digitalen Computern. In der analogen Computertechnik war das Runden ein physischer Prozess. In der digitalen Computertechnik ist es ein mathematischer.

Runden ist erforderlich, wenn wir von einem Typ mit höherer Präzision (wie `float` oder `double`) zu einem Typ mit geringerer Präzision (wie `int`) konvertieren. Aber wie wir runden, kann variieren:

1. `round()`: Standardrundung. Wenn der Bruchteil 0,5 oder höher ist, geht es auf; sonst geht es ab.
2. `ceil()`: Kurz für "ceiling", rundet immer auf die nächste ganze Zahl auf, auch wenn sie näher an der niedrigeren Zahl liegt.
3. `floor()`: Das Gegenteil von ceiling; rundet immer ab.

Die Wahl zwischen diesen Funktionen hängt davon ab, wofür der gerundete Wert ist. Für Messungen könnte Standardrundung benötigt werden, Geld verwendet oft `floor`, während Inventarsysteme `ceil` verwenden könnten, um sicherzustellen, dass alles berücksichtigt wird.

Die Implementierung dieser Funktionen in Arduino ist unkompliziert; sie behandeln keine zusätzlichen Fälle wie das Runden auf spezifische Dezimalstellen. Dafür kommt eine benutzerdefinierte Funktion oder tiefere Mathematik ins Spiel – denken Sie daran, zu multiplizieren, um das Dezimal zu verschieben, zu runden und dann zurückzuteilen.

Rundungsfehler können sich ansammeln und langwierige Berechnungen oder iterative Prozesse erheblich beeinflussen. Programmierer müssen vorsichtig sein, wenn sie zahlreiche Operationen mit gerundeten Werten durchführen.

## Siehe auch:
2. Detaillierter Blick auf die Fallstricke und Strategien zum Runden: [Floating Point Guide](https://floating-point-gui.de/)
3. Für fortgeschrittene Techniken, einschließlich benutzerdefinierter Rundungsfunktionen und Umgang mit Rundungsfehlern, könnten Sie akademische Ressourcen oder detaillierte Programmierleitfäden überprüfen.
