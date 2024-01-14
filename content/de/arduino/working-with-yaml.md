---
title:                "Arduino: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Warum

In dieser Blog-Post wollen wir uns mit der Arbeit von YAML in Verbindung mit Arduino beschäftigen. YAML steht für "YAML Ain't Markup Language" und ist eine einfache und leicht lesbare Formatierungssprache. Sie wird häufig verwendet, um Konfigurationsdateien zu speichern, aber sie kann auch in der Programmierung nützlich sein. Lesen Sie weiter, um zu erfahren, wie Sie YAML mit Arduino verwenden können.

## Wie geht das?

```Arduino
#include <YAML.h>

void setup() {
  Serial.begin(9600);
  YAML.begin();
}

void loop() {
  YAML.print("Hallo, Welt!");
  YAML.print(123);
  YAML.println(true);
  delay(1000);
}
```

Der obige Code zeigt, wie man die Arduino YAML Bibliothek einbindet und wie man sie verwendet, um "Hallo, Welt!" sowie eine Zahl und einen booleschen Wert an den seriellen Monitor auszugeben.

Die Ausgabe des obigen Programms sollte wie folgt aussehen:

```
Hallo, Welt!123true
```

Sie können auch Variablen oder Ausdrücke anstelle von statischen Werten in die `print()` Funktion einfügen. Hier ist ein Beispiel:

```Arduino
#include <YAML.h>

int num = 5;

void setup() {
  Serial.begin(9600);
  YAML.begin();
}

void loop() {
  num++;
  YAML.print("Die Zahl ist jetzt: ");
  YAML.println(num);
  delay(1000);
}
```

Die Ausgabe dieses Programms wäre:

```
Die Zahl ist jetzt: 6
Die Zahl ist jetzt: 7
Die Zahl ist jetzt: 8
usw.
```

## Tiefer eintauchen

Wie bereits erwähnt, ist YAML eine einfache und leicht lesbare Formatierungssprache. Es gibt jedoch eine Reihe von Regeln, die man beachten sollte, wenn man damit arbeitet. Zum Beispiel müssen alle Werte durch einen Doppelpunkt abgetrennt werden und es dürfen keine Tabs verwendet werden, sondern nur Leerzeichen zur Einrückung. Es gibt auch Möglichkeiten, Objekte und Arrays innerhalb von YAML zu erstellen, die in der offiziellen Dokumentation genauer beschrieben werden.

Eine Sache, die es bei der Arbeit mit YAML und Arduino zu beachten gilt, ist, dass einige Zeichen wie Anführungszeichen und Backslashes escape-fähig sind. Das bedeutet, dass sie eine spezielle Bedeutung haben und mit einem Backslash (\) gekennzeichnet werden müssen, wenn sie selbst als Wert verwendet werden sollen.

## Siehe auch

- [Offizielle YAML Dokumentation](https://yaml.org/)
- [Arduino YAML Bibliothek auf GitHub](https://github.com/ErikPDev/arduino-yaml)
- [YAML Tutorial für Anfänger](https://www.liquid-technologies.com/Tutorial/yaml-tutorial)