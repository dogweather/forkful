---
title:    "C: Erstellen einer temporären Datei"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Warum

Das Erstellen von temporären Dateien ist eine häufige Aufgabe beim Programmieren in C. Es kann nützlich sein, um Daten zwischen verschiedenen Funktionen oder Programmen auszutauschen. Außerdem können temporäre Dateien verwendet werden, um vorübergehend große Mengen an Daten zu speichern, ohne den Speicher des Computers zu belasten.

# Wie man eine temporäre Datei erstellt

Um eine temporäre Datei in C zu erstellen, müssen wir drei wichtige Schritte durchführen: Erstellen, Öffnen und Schließen der Datei.

```C 
// Schritt 1: Datei erstellen
FILE *file = tmpfile();
if (file == NULL) {
    printf("Fehler beim Erstellen der Datei!");
    exit(1);
}

// Schritt 2: Datei öffnen
int file_des = fileno(file); // konvertiert FILE-Pointer in Dateideskriptor
FILE *fp = fdopen(file_des, "w"); // Datei im Schreibmodus öffnen

// Schritt 3: Daten in die Datei schreiben
fprintf(fp, "Hallo, Welt!");

// Datei schließen
fclose(fp);
```

Die `tmpfile()`-Funktion erstellt eine temporäre Datei im System-Temp-Verzeichnis und gibt einen FILE-Pointer zurück. Wir prüfen dann, ob die Datei erfolgreich erstellt wurde und öffnen sie im Schreibmodus. Mithilfe von `fprintf()` können wir Daten in die Datei schreiben. Zum Schluss wird die Datei geschlossen, um den Speicher freizugeben.

# Tiefere Einblicke

Das Erstellen einer temporären Datei ist in der Regel eine effiziente Methode, um Daten temporär zu speichern. Allerdings gibt es einige Punkte zu beachten:

- Temporäre Dateien werden automatisch gelöscht, sobald das Programm beendet wird. Daher sollten sie nicht verwendet werden, um permanent Daten zu speichern.

- Temporäre Dateien können von anderen Programmen oder Benutzern gelesen werden, da sie im System-Temp-Verzeichnis gespeichert werden. Um dies zu verhindern, sollten wir `mkstemp()` verwenden, um eine temporäre Datei mit zugriffsbeschränkten Berechtigungen zu erstellen.

- Das Erstellen von temporären Dateien kann auf einigen Systemen, insbesondere in eingebetteten Systemen, nicht unterstützt werden. In diesen Fällen sollten wir alternative Methoden zur Speicherung temporärer Daten wie dynamische Speicherzuweisung verwenden.

# Siehe auch

- [C - Dateien und Streams](https://www.tutorialspoint.com/c_standard_library/c_function_freopen.htm)
- [mkstemp() Funktion](https://www.man7.org/linux/man-pages/man3/tmpnam.3.html)