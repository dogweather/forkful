---
date: 2024-01-20 17:34:01.263593-07:00
description: "String-Konkatenation ist das Verbinden von zwei oder mehr Zeichenketten\
  \ zu einer. Wir nutzen dies, um dynamische Texte zu erzeugen oder Daten in lesbaren\u2026"
lastmod: '2024-03-13T22:44:54.136983-06:00'
model: gpt-4-1106-preview
summary: "String-Konkatenation ist das Verbinden von zwei oder mehr Zeichenketten\
  \ zu einer. Wir nutzen dies, um dynamische Texte zu erzeugen oder Daten in lesbaren\u2026"
title: "Zeichenketten verkn\xFCpfen"
---

{{< edit_this_page >}}

## Was & Warum?

String-Konkatenation ist das Verbinden von zwei oder mehr Zeichenketten zu einer. Wir nutzen dies, um dynamische Texte zu erzeugen oder Daten in lesbaren Formaten auszugeben.

## So geht's:

```Arduino
void setup() {
  Serial.begin(9600);

  String gruss = "Hallo";
  String name = "Welt";
  String satz = gruss + ", " + name + "!"; // Konkatenation

  Serial.println(satz); // Gibt "Hallo, Welt!" aus
}

void loop() {
  // Nichts zu tun hier
}
```

Ausgabe:
```
Hallo, Welt!
```

## Im Detail:

String-Konkatenation ist keine neue Idee. In C, der "Großvater" der Arduino-Programmiersprache, wurden Zeichenketten mit der strcat-Funktion verbunden. In Arduino verwenden wir jedoch die einfache `+`-Operation, um den Code leserlicher und effizienter zu gestalten. Alternativ könnten wir die `concat()`-Methode der String-Objekte nutzen, oder in Fällen, wo Speichernutzung kritisch ist, char-Arrays mit `strcat()` verwenden. Bei der Verwendung von `+` sollte man jedoch aufpassen, da dies bei jedem Vorgang ein neues String-Objekt erzeugt und damit den Speicher fragmentieren kann.

## Siehe Auch:

- Arduino Referenz zu Strings: https://www.arduino.cc/reference/en/language/variables/data-types/string/
- Eine Diskussion über die Effizienz von String-Konkatenation auf Stack Overflow: https://stackoverflow.com/questions/7293236/efficient-string-concatenation-in-c
- Tutorial zur Arbeit mit char-Arrays in Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strcat/
