---
date: 2024-01-20 17:49:59.973310-07:00
description: 'How to: Ausgabe: `Die aktuelle Temperatur ist: 23 Grad Celsius`.'
lastmod: '2024-04-05T22:38:54.987741-06:00'
model: gpt-4-1106-preview
summary: 'Ausgabe: `Die aktuelle Temperatur ist: 23 Grad Celsius`.'
title: Zeichenketten interpolieren
weight: 8
---

## How to:
```Arduino
char temperaturMsg[50];
int temperatur = 23;

void setup() {
  Serial.begin(9600);
}

void loop() {
  sprintf(temperaturMsg, "Die aktuelle Temperatur ist: %d Grad Celsius", temperatur);
  Serial.println(temperaturMsg);
  delay(2000); // Update alle 2 Sekunden
}
```
Ausgabe: `Die aktuelle Temperatur ist: 23 Grad Celsius`

## Deep Dive:
String-Interpolation ist kein neues Konzept und wurde in früheren Programmiersprachen wie Perl und Python populär. 

In Arduino wird keine direkte Interpolation wie in einigen modernen Sprachen unterstützt. Stattdessen verwendet man `sprintf()`, eine Standard-C-Funktion, um Variablen in Strings einzufügen.

Der `%d` im `sprintf` ist ein Platzhalter für Ganzzahlwerte. Für andere Datentypen gibt es andere Platzhalter, wie `%f` für Fließkommazahlen oder `%s` für Strings.

Beachte aber, dass Arduino's `sprintf` nicht mit Gleitkommazahlen (`float`, `double`) arbeitet. Man muss zuerst in eine ganzzahlige Darstellung umwandeln, wenn Fließkomma erforderlich ist.

Alternativen sind die Verwendung der `String`-Klasse mit dem `+` Operator oder `String`-Methoden wie `concat()`. Diese Methoden sind aber weniger effizient im Speicherverbrauch.

## See Also:
- Arduino-Referenz für `sprintf`: https://www.arduino.cc/reference/en/language/functions/characters/printf/
- `String`-Klasse in Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
