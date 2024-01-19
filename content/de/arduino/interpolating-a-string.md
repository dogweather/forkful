---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Arduino: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

String-Interpolation ermöglicht das Einbetten von Ausdrücken oder Variablenwerten direkt in Zeichenketten. Programmierer setzen sie ein, weil es das Erstellen komplexer Zeichenketten vereinfacht und für bessere Lesbarkeit und Wartbarkeit sorgt.

## Wie geht das:

Nun, Arduino unterstützt nicht direkt die String-Interpolation, aber wir können etwas Ähnliches erreichen. Sehen wir uns ein simples Beispiel an:

```Arduino
String name = "Hans";
int age = 32;

Serial.begin(9600);
Serial.println("Name: " + name + ", Alter: " + age);

```

Ausgabe wird sein:

```Arduino
Name: Hans, Alter: 32
```


## Vertiefung:

Historisch gesehen gab es in Sprachen, die String-Interpolation unterstützten (wie Perl oder Ruby), immer eine Methode, diese Funktion zu nutzen. Bei Arduino kann man bisher nur durch string concatenation ein ähnlicher Effekt erreichen.

Alternativ könnten Sie für komplizierte Formate die sprintf-Funktion verwenden, die aber in einem eingeschränkten Format zur Verfügung steht.

Zu Einzelheiten einer Implementierung - Die String "Addition" in Arduino ruft die concat() Funktion der String-Klasse auf. 

## Siehe auch:

- [Arduino String Reference (Englisch)](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Arduino sprintf-Funktion (Englisch)](http://www.cplusplus.com/reference/cstdio/sprintf/)
- [Arduino-Datenblätter und Handbücher (Deutsch)](https://www.arduino.cc/en/Main/Documentation)