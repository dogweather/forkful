---
title:    "Arduino: Erstellung einer temporären Datei"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Warum

In diesem Beitrag werden wir uns damit beschäftigen, wie man in Arduino temporäre Dateien erstellt. Doch warum sollte man überhaupt temporäre Dateien erstellen? Temporäre Dateien dienen dazu, vorübergehend Daten zu speichern, die später nicht mehr benötigt werden. Das kann zum Beispiel sinnvoll sein, wenn man während eines Programmablaufs Zwischenergebnisse speichern und später wieder löschen möchte.

# So funktioniert es

Die Erstellung einer temporären Datei kann mit der `File`-Bibliothek von Arduino durchgeführt werden. Dazu müssen zunächst die entsprechenden Bibliotheken in den Sketch importiert werden.

```Arduino
#include <SD.h>
#include <SPI.h>
```

Danach kann man mit der Funktion `SD.open()` eine temporäre Datei erstellen und ihr einen Namen sowie den Dateimodus zuweisen. Der Dateiname kann dabei frei gewählt werden und der Dateimodus wird mit `FILE_WRITE` festgelegt.

```Arduino
File tempFile = SD.open("temp.txt", FILE_WRITE);
```

Nun können Daten in die temporäre Datei geschrieben werden, zum Beispiel mit der `println()` Funktion.

```Arduino
tempFile.println("Dies ist ein Beispieltext.");
```

Zuletzt muss die Datei wieder geschlossen und die Verbindung zum Speichermedium getrennt werden.

```Arduino
tempFile.close();
SD.end();
```

# Tiefere Einblicke

Ein wichtiger Aspekt bei der Erstellung temporärer Dateien ist die Verwendung von eindeutigen Dateinamen. Es empfiehlt sich, eine Zufallszahl oder einen Zeitstempel in den Dateinamen mit einzubauen, um sicherzustellen, dass es keine Namenskonflikte gibt.

Außerdem ist es wichtig, die temporären Dateien regelmäßig zu löschen, um Speicherplatz zu sparen. Dazu kann man zum Beispiel eine Schleife nutzen, die alle temporären Dateien durchgeht und löscht. Eine weitere Möglichkeit ist die Verwendung einer Funktion wie `SD.exists()`, um zu überprüfen, ob eine bestimmte temporäre Datei vorhanden ist, bevor sie gelöscht wird.

# Siehe auch

- Website über die `File`-Bibliothek von Arduino: https://www.arduino.cc/en/Reference/SD
- Weitere Informationen zum Thema temporäre Dateien: https://de.wikipedia.org/wiki/Tempor%C3%A4re_Datei