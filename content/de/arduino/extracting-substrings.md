---
title:                "Unterscheiden von Teilstrings"
html_title:           "Arduino: Unterscheiden von Teilstrings"
simple_title:         "Unterscheiden von Teilstrings"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Extrahieren von Teilzeichenfolgen bezieht sich darauf, einen Teil einer größeren Zeichenfolge zu isolieren. Programmierer tun dies, um bestimmte Informationen aus Texten oder Variablen zu extrahieren und für ihre Zwecke zu verwenden.

# Wie geht's?
Die Extraktion von Teilzeichenfolgen kann in Arduino mithilfe der Funktion `substring()` durchgeführt werden. Diese Funktion erfordert zwei Parameter - den Index des ersten und den Index des letzten Zeichens, die extrahiert werden sollen. Zum Beispiel:

```
Arduino void setup() {
  String text = "Hallo Welt!";
  String substring = text.substring(3,7);
  Serial.println(substring);
}
```

Dieses Beispiel verwendet die Zeichenfolge "Hallo Welt!" und extrahiert die Teilzeichenfolge "lo W", da der Index des ersten Zeichens "l" (3) und der letzte Index "t" (6) angegeben ist.

# Tief eintauchen
Die Funktion `substring()` wurde erstmals in der Programmiersprache Java eingeführt und ist auch in anderen Programmiersprachen wie C++, Python und JavaScript verfügbar. Alternativ können Entwickler auch die Funktionen `charAt()` und `split()` verwenden, um Teilzeichenfolgen in Arduino zu extrahieren. Es gibt auch Bibliotheken, wie z.B. die BearSSL-Bibliothek, die für die Extraktion von Teilzeichenfolgen in verschlüsselten Nachrichten verwendet werden kann.

# Siehe auch
- Dokumentation zu `substring()`: https://www.arduino.cc/reference/de/language/variables/data-types/string/functions/substring/
- Die BearSSL-Bibliothek: https://www.arduino.cc/reference/en/libraries/bearssl/
- Eine Einführung in Strings in Arduino: https://www.arduino.cc/en/Tutorial/StringObject