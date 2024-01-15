---
title:                "Arbeiten mit YAML"
html_title:           "Arduino: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

# Warum

Wenn du mit Arduino arbeitest, kann es hilfreich sein, Daten in einer übersichtlichen und strukturierten Form zu speichern. YAML ist ein einfaches Format, das dir dabei helfen kann, verschiedene Datentypen wie Zahlen, Texte und Listen zu organisieren.

# Wie geht's

Die Integration von YAML in deine Arduino-Projekte ist sehr einfach. Hier ist ein Beispiel, wie du eine Liste von Sensorwerten in YAML speichern kannst:

```Arduino
#include <ArduinoJson.h>
#include <YAML.h>

void setup() {
  YAML::Object data;
  data["sensor1"] = analogRead(A0);
  data["sensor2"] = analogRead(A1);
  data["sensor3"] = analogRead(A2);

  String yamlString;
  YAML::Emitter emitter(yamlString);
  emitter << data;

  Serial.begin(9600);
  Serial.println(yamlString);
}
```

Und das ist das Ergebnis, das du im Seriellen Monitor sehen wirst:

```Arduino
sensor1: 18
sensor2: 240
sensor3: 551
```

Wie du sehen kannst, sind die Werte in einer strukturierten und lesbaren Form gespeichert. Du kannst auch komplexere Datenstrukturen wie Arrays und Objekte in YAML speichern. Hier ist ein Beispiel, wie du ein Array von Temperaturwerten speichern kannst:

```Arduino
#include <ArduinoJson.h>
#include <YAML.h>

#define NUM_SENSORS 5

void setup() {
  float tempArray[NUM_SENSORS] = {22.5, 23.3, 21.7, 24.8, 20.9};
  
  YAML::Object data;
  data["temperatures"] = tempArray;

  String yamlString;
  YAML::Emitter emitter(yamlString);
  emitter << data;

  Serial.begin(9600);
  Serial.println(yamlString);
}
```

Und hier ist das Ergebnis:

```Arduino
temperatures: [22.5, 23.3, 21.7, 24.8, 20.9]
```

# Tiefer eintauchen

Wenn du mehr über YAML erfahren möchtest, gibt es einige wichtige Konzepte, die du verstehen solltest. YAML verwendet Einrückungen, um Datenstrukturen zu definieren. Zum Beispiel:

```yaml
- Sensor1:
    pin: A0
    type: analog
- Sensor2:
    pin: A1
    type: analog
```

Dies ist ein Array von Objekten, die jeweils aus zwei Schlüssel-Wert-Paaren bestehen. Die Einrückung zeigt, dass die Schlüssel "Pin" und "Typ" zum jeweiligen Sensorobjekt gehören.

Eine weitere wichtige Sache ist, dass YAML dynamisch typisiert ist. Das bedeutet, dass du nicht angeben musst, welche Datentypen du speichern möchtest. YAML erkennt automatisch, ob ein Wert eine Zahl, ein Text oder eine Liste ist und kann entsprechend darauf zugreifen.

Für weitere Informationen und Beispiele kannst du die offizielle YAML-Dokumentation lesen.

# Siehe auch

- [Offizielle YAML-Dokumentation](https://yaml.org/)
- [ArduinoJson Library](https://arduinojson.org/)
- [YAML Library für Arduino](https://github.com/inaciose/yaml-arduino)