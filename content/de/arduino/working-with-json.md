---
title:                "Arduino: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit der JSON-Programmierung auf Arduino beschäftigen? Ganz einfach: JSON (JavaScript Object Notation) ist ein weit verbreitetes Format für den Datenaustausch und wird von vielen APIs und Webanwendungen verwendet. Durch die Verwendung von JSON in deinen Projekten kannst du somit Daten aus verschiedenen Quellen einbinden und verarbeiten.

## Wie man JSON auf Arduino verwendet

Die Verwendung von JSON auf Arduino kann in drei Schritten erfolgen: Parsing, Verarbeitung und Formatierung.

### Parsing

Zunächst muss man die JSON-Daten in ein geeignetes Format für Arduino umwandeln. Dies kann entweder manuell erfolgen oder mithilfe von Bibliotheken wie zum Beispiel ArduinoJson.

Ein Beispiel für die manuelle Konvertierung könnte so aussehen:

```
String json = "{\"name\": \"Max\", \"age\": 25}";
// Lese den Wert des "name"-Schlüssels aus
String name = json.substring(json.indexOf(":") + 2, json.indexOf(",") - 1);
// Lese den Wert des "age"-Schlüssels aus
String age = json.substring(json.indexOf(",", json.indexOf(",") + 1) + 2, json.length() - 1);
```

### Verarbeitung

Nachdem die Daten geparst wurden, können sie nun verarbeitet werden. Dies kann je nach Anwendungsfall unterschiedlich aussehen. Ein Beispiel für die Verarbeitung könnte das Ausgeben der Daten auf einem Display sein:

```
display.print("Name: " + name);
display.print("Alter: " + age);
```

### Formatierung

Zuletzt muss man die Daten noch in das gewünschte Format bringen. Auch hier gibt es verschiedene Möglichkeiten, je nachdem was mit den Daten gemacht werden soll. Hier ein Beispiel, wie man die Daten als JSON-String wieder ausgeben könnte:

```
String output = "{\"name\": \"" + name + "\", \"age\": " + age + "}";
```

## Tiefer Einblick

Möchtest du tiefer in die Welt der JSON-Programmierung auf Arduino eintauchen? Dann solltest du dich mit den verschiedenen Datentypen und Funktionen von JSON vertraut machen. Hier einige Links, die dir dabei helfen können:

- [ArduinoJson Dokumentation](https://arduinojson.org/v6/api/)
- [JSON-Referenz](https://www.json.org/json-de.html)
- [Wie man mit JSON auf Arduino arbeitet](https://randomnerdtutorials.com/sdk-content-mg-rs/)
- [Beispiele für die Verwendung von ArduinoJson](https://github.com/bblanchon/ArduinoJson/tree/6.x/examples)

## Siehe auch

Weitere interessante Artikel zum Thema Arduino-Programmierung finden Sie unter folgenden Links:

- [Wie man eine LED mit Arduino steuert](https://www.tinkerunity.org/wiki/index.php/LED_mit_Arduino_steurn)
- [Einsteiger-Anleitung für Arduino](https://www.heise.de/select/make/2017/5/1493499952132886)
- [Arduino Projektideen](https://create.arduino.cc/projecthub)
- [Schreib- und Lesezugriffe mit Arduino](https://www.opencaching.de/wiki/index.php/Arduino_und_SPI-Bus)