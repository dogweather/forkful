---
title:    "Arduino: Überprüfung der Existenz eines Verzeichnisses"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Warum

Wenn Sie schon einmal ein Arduino-Projekt entwickelt haben, haben Sie vielleicht bemerkt, dass es manchmal notwendig ist, das Vorhandensein eines bestimmten Verzeichnisses auf Ihrem Board zu überprüfen. Dies kann aus verschiedenen Gründen erforderlich sein, zum Beispiel um zu überprüfen, ob eine Datei vorhanden ist, bevor Sie versuchen, sie zu öffnen. In diesem Blog-Beitrag werden wir uns genauer ansehen, wie Sie auf einem Arduino prüfen können, ob ein bestimmtes Verzeichnis vorhanden ist.

## So geht's

Um das Vorhandensein eines Verzeichnisses auf Ihrem Arduino-Board zu überprüfen, können Sie die `SD.exists()` Funktion verwenden. Diese Funktion benötigt eine Zeichenfolge als Argument, die den Pfad des Verzeichnisses darstellt, das Sie überprüfen möchten. Zum Beispiel:

```
#include <SD.h>

void setup() {
  // Initialisiere die SD-Karte
  SD.begin(4);
  
  // Überprüfe, ob das Verzeichnis "logs" vorhanden ist
  if (SD.exists("/logs")) {
    Serial.println("Das Verzeichnis 'logs' existiert auf der SD-Karte.");
  } else {
    Serial.println("Das Verzeichnis 'logs' existiert nicht auf der SD-Karte.");
  }
}

void loop() {
  // Hier können weitere Aktionen durchgeführt werden
}
```

Je nachdem, ob das Verzeichnis existiert oder nicht, wird eine entsprechende Meldung auf der seriellen Monitor ausgegeben. Beachten Sie, dass Sie die SD-Bibliothek zuerst mit dem Befehl `#include <SD.h>` importieren müssen, bevor Sie die `SD.exists()` Funktion verwenden können.

## Tiefergehende Informationen

Wenn Sie tiefer in die Funktionsweise von `SD.exists()` eintauchen möchten, können Sie sich die Dokumentation des SD-Typedefs ansehen. Dort finden Sie weitere Informationen über die verschiedenen Funktionen, die für die Überprüfung von Verzeichnissen auf der SD-Karte zur Verfügung stehen.

## Siehe auch

- Dokumentation des SD-Typedefs: https://www.arduino.cc/en/Reference/SD
- Tutorial zur Verwendung der SD-Bibliothek: https://randomnerdtutorials.com/complete-guide-for-basics-of-sd-card-module-with-arduino/