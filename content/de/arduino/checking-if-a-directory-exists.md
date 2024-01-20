---
title:                "Überprüfung, ob ein Verzeichnis existiert"
html_title:           "Arduino: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Überprüfung, ob ein Verzeichnis existiert, bezieht sich auf den Prozess der Verifizierung der Existenz eines bestimmten Ordners in einem Speichergerät mit der Arduino-Software. Dies ist nützlich, um Fehler zu vermeiden, die auftreten können, wenn wir versuchen, auf ein nicht existierendes Verzeichnis zuzugreifen.

## So geht's:

Wir nutzen den SPIFFS (SPI Flash File System) in der ESP8266 Arduino-Core und überprüfen die Existenz des Verzeichnisses anhand folgender Beispiele. 

```Arduino
#include "FS.h"

void setup() {
  Serial.begin(115200);
  
  if (!SPIFFS.begin()) {
    Serial.println("An Error has occurred while mounting SPIFFS");
    return;
  }

  if(SPIFFS.exists("/myDir")){
    Serial.println("Directory exists!");
  }
  else{
    Serial.println("Directory doesn't exist!");
  }
}

void loop() {
  
}
```

In diesem Code erstellt `SPIFFS.begin()` das Dateisystem, während `SPIFFS.exists("/myDir")` überprüft, ob das Verzeichnis existiert oder nicht. Der resultierende Ausdruck wird dann auf der seriellen Konsole ausgegeben.

## Tiefgreifende Informationen

Die Funktion `SPIFFS.exists()` wurde in frühen Arduino-Versionen eingeführt, um die Interaktion mit dem Dateisystem zu erleichtern. Alternativ können Sie die `File`-Klasse und ihre `open()`-Methode verwenden, die jedoch mehr Code und Komplexität erfordert.

Die Überprüfung, ob ein Verzeichnis existiert, ist hilfreich, um Unsicherheiten in Bezug auf die Speicherstatus Ihrer Programme zu reduzieren.	Dies gewährleistet, dass Ihr Programm nicht versucht, Dateien in einem nicht existierenden Verzeichnis zu speichern oder von dort zu lesen, was zu Laufzeitfehlern führen könnte.

## Siehe auch

Für weitere Details und Beispiele zur Interaktion mit dem Dateisystem auf der ESP8266 Arduino Core, besuchen Sie die Dokumentationsseite unter [https://arduino-esp8266.readthedocs.io/en/latest/filesystem.html](https://arduino-esp8266.readthedocs.io/en/latest/filesystem.html).

Für eine gründlichere Erklärung der `SPIFFS.exists()`-Methode, besuchen Sie die Arduino Reference unter [https://www.arduino.cc/en/Reference/SPISFFSexists](https://www.arduino.cc/en/Reference/SPISFFSexists).

Ein Verständnis dieser Funktionen und Praktiken gibt Ihnen mehr Kontrolle und Sicherheit beim Programmieren mit Arduino.