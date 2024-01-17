---
title:                "Verkettung von Zeichenketten"
html_title:           "Arduino: Verkettung von Zeichenketten"
simple_title:         "Verkettung von Zeichenketten"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was ist das und warum sollte man es tun?

Das Zusammenfügen von Strings ist eine häufige Aufgabe in der Programmierung, bei der verschiedene Texte oder Zeichenketten miteinander verbunden werden. Das kann nützlich sein, um beispielsweise eine längere Nachricht oder einen variablen Text zu erstellen. Programmierer nutzen das Verfahren, um ihre Code-Effizienz zu steigern und repetitive Aufgaben zu vermeiden.

## Wie funktioniert es?

Das Zusammenfügen von Strings in einem Arduino Sketch ist einfach und erfordert nur die Verwendung des ```+``` Operators. In folgendem Beispiel wird der Text "Hallo " mit einer Variable ```name``` kombiniert und dann auf dem seriellen Monitor ausgegeben:

```Arduino
String name = "Max";
Serial.println("Hallo " + name);
```

Die Ausgabe würde dann "Hallo Max" sein. Eine weitere Möglichkeit ist die Verwendung der Funktion ```concat()```, die speziell für das Zusammenfügen von Strings in Arduino entwickelt wurde. Zum Beispiel:

```Arduino
String nachname = "Mustermann";
String vollerName;
vollerName.concat("Max");
vollerName.concat(" ");
vollerName.concat(nachname);
Serial.println(vollerName);
```

Die Ausgabe wäre wiederum "Max Mustermann".

## Tiefere Einblicke

Das Zusammenfügen von Strings ist keine neue Technik und wird bereits seit den frühen Tagen der Programmierung verwendet. In der Arduino-Plattform können auch andere Datentypen wie Zahlen oder Charaktere mit Strings zusammengefügt werden, was die Möglichkeiten erweitert.

Alternativ können Programmierer auch Arrays von Characters (unter Verwendung der Funktion ```strcpy()```) oder die Bibliothek StringOperations nutzen, um Strings zusammenzufügen.

Die Implementierung des Zusammenfügens von Strings in Arduino erfolgt über unterschiedliche Funktionen und Methoden, die je nach Verwendungszweck ausgewählt werden können. Es ist wichtig sicherzustellen, dass genügend Speicherplatz vorhanden ist, da eine große Anzahl von Zusammenfügungen zu einem Fehler führen kann.

## Weitere Informationen

Weitere Informationen zum Zusammenfügen von Strings in Arduino finden Sie in der offiziellen Dokumentation unter [https://www.arduino.cc/reference/tr/language/variables/data-types/stringconcat/](https://www.arduino.cc/reference/tr/language/variables/data-types/stringconcat/).

Weitere nützliche Ressourcen sind das offizielle Arduino Forum [https://forum.arduino.cc/index.php?board=1.0](https://forum.arduino.cc/index.php?board=1.0), in dem Sie Fragen stellen und Antworten finden können, sowie die Entwickler-Community [https://www.arduino.cc](https://www.arduino.cc) mit Tutorials und Beispielen für Anfänger und Fortgeschrittene.

 Viel Spaß beim Programmieren und Happy Hacking!