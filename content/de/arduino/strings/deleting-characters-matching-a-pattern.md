---
date: 2024-01-20 17:41:33.742545-07:00
description: "So geht's: Das L\xF6schen von Zeichen nach einem Muster ist nichts Neues\
  \ und stammt aus der Zeit vor Arduino, als Textverarbeitungen in der\u2026"
lastmod: '2024-04-05T21:53:56.012075-06:00'
model: gpt-4-1106-preview
summary: "Das L\xF6schen von Zeichen nach einem Muster ist nichts Neues und stammt\
  \ aus der Zeit vor Arduino, als Textverarbeitungen in der Softwareentwicklung \xFC\
  blich wurden."
title: "L\xF6schen von Zeichen, die einem Muster entsprechen"
weight: 5
---

## So geht's:
```Arduino
String text = "A1r2d3u4i5n6o789";
String muster = "123456789";

void setup() {
  Serial.begin(9600);
  String bereinigterText = musterEntfernen(text, muster);
  Serial.println(bereinigterText); // Gibt "Arduino" aus
}

void loop() {
  // Nichts zu tun hier
}

String musterEntfernen(String quelle, String muster) {
  for (int i = 0; i < muster.length(); i++) {
    quelle.replace(String(muster[i]), "");
  }
  return quelle;
}
```

## Tieferes Verständnis
Das Löschen von Zeichen nach einem Muster ist nichts Neues und stammt aus der Zeit vor Arduino, als Textverarbeitungen in der Softwareentwicklung üblich wurden. In anderen Sprachen gibt es oft eingebaute Funktionen wie regex (reguläre Ausdrücke), die das noch mächtiger machen. Arduinos String-Klasse ist einfacher, aber in vielen Fällen ausreichend. Die `replace()`-Funktion, wie im Beispiel genutzt, ist einfach zu verstehen und reicht für viele Musterlöschbedürfnisse aus. Bei der Arbeit mit Mikrokontrollern wie dem Arduino sollte man auf den Speicherverbrauch achten – große oder komplexe Muster können den begrenzten Speicher rasch füllen.

## Siehe Auch
- Arduino String Reference: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Mehr über reguläre Ausdrücke: https://www.regular-expressions.info/
- Details zur Speicherverwaltung auf Arduino: https://www.arduino.cc/en/Tutorial/Foundations/Memory
