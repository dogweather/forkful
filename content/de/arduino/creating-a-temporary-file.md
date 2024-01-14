---
title:                "Arduino: Erstellen einer temporären Datei"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Warum

Warum sollte man sich mit der Erstellung einer temporären Datei befassen? Nun, temporäre Dateien können dabei helfen, Daten temporär zu speichern oder zu übertragen, ohne den permanenten Speicherplatz des Systems zu beanspruchen. Sie können auch eingesetzt werden, um komplexe Berechnungen oder Prozesse zu unterstützen, die erst in einem späteren Schritt in einer permanenten Datei gespeichert werden sollen.

# Wie geht man vor

Um eine temporäre Datei in Arduino zu erstellen, kann die Funktion "tempfile()" verwendet werden. Hier ist ein Beispielcode:

```
Arduino char filename[13]; // Name der temporären Datei wird in ein Char-Array geschrieben
File tempFile; // Neue temporäre Datei wird erstellt
tempFile = tempfile(filename); // Die Funktion erstellt die temporäre Datei
tempFile.print("Hallo Welt!"); // Der Inhalt wird in die temporäre Datei geschrieben
tempFile.close(); // Die Datei wird geschlossen
```

Um den Inhalt der temporären Datei zu überprüfen, kann die Funktion "open()" verwendet werden. Hier ist ein weiteres Beispiel:

```
Arduino char filename[13]; // Name der temporären Datei wird in ein Char-Array geschrieben
File tempFile; // Neue temporäre Datei wird erstellt
tempFile = tempfile(filename); // Die Funktion erstellt die temporäre Datei
tempFile.open(filename); // Die temporäre Datei wird geöffnet
while (tempFile.available()) { // Solange noch Inhalte verfügbar sind
  char data = tempFile.read(); // Daten werden einzeln ausgelesen
  Serial.print(data); // Daten werden in der Seriellen Monitor ausgegeben
}
tempFile.close(); // Die Datei wird geschlossen
```

# Tiefere Einblicke

Um die Verwendung und Erstellung von temporären Dateien besser zu verstehen, ist es wichtig, sich mit den Unterschieden zwischen temporären und permanenten Dateien auseinanderzusetzen. Während permanente Dateien dauerhaft auf dem Speicher des Systems gespeichert werden, sind temporäre Dateien nur für eine begrenzte Zeit verfügbar und werden danach gelöscht.

Zusätzlich gibt es verschiedene Arten von temporären Dateien, wie z.B. "volatile" oder "shared". Diese haben jeweils unterschiedliche Eigenschaften und Zwecke.

# Siehe auch

- [Arduino Dokumentation zu "tempfile()"](https://www.arduino.cc/reference/de/language/functions/file-io/tempfile/)
- [Wikipedia Eintrag zu "Temporäre Datei"](https://de.wikipedia.org/wiki/Tempor%C3%A4re_Datei)