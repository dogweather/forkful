---
title:                "Erstellen einer temporären Datei"
html_title:           "C++: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Erstellen einer temporären Datei ist eine häufige Aufgabe für Programmierer. Es ermöglicht ihnen, temporäre Daten in einer Datei zu speichern, die nach dem Gebrauch automatisch gelöscht wird. Dies ist besonders nützlich, um temporäre Ergebnisse oder Zwischenspeicherungen in einem Programm zu verarbeiten.

# Wie geht's?
Das Erstellen einer temporären Datei in C++ ist ziemlich einfach. Sie müssen lediglich die C++-Standardbibliotheksfunktion "tmpfile" aufrufen, die eine temporäre Datei in einem vom Betriebssystem verwalteten temporären Verzeichnis erstellt. Schauen wir uns dazu ein Beispiel an:

```C++
#include <cstdio>

int main() {
    FILE* tmpf = tmpfile();
    if (tmpf != nullptr) {
        fputs("Dies ist ein Beispieltext", tmpf);
        rewind(tmpf);
        char buffer[256];
        while (fgets(buffer, 256, tmpf) != nullptr) {
            puts(buffer);
        }
        fclose(tmpf);
    }
    return 0;
}
```

Dieses Beispiel zeigt, wie Sie eine temporäre Datei erstellen, eine Zeile in die Datei schreiben und dann den Inhalt der Datei auslesen und auf dem Bildschirm ausgeben können.

Die Ausgabe des obigen Programms wäre: "Dies ist ein Beispieltext".

# Tiefer Einblick
Das Konzept der temporären Dateien gibt es schon seit langem und wird von Betriebssystemen zur Verfügung gestellt, um Programme dabei zu unterstützen, temporäre Daten zu verarbeiten. In C++ gibt es neben "tmpfile" auch die Funktionen "tmpnam" und "tempnam", die auf unterschiedliche Weise temporäre Dateien erstellen.

Alternativ können Programmierer auch beschreiben, wie und wo sie ihre temporären Dateien erstellen möchten, anstatt vom Betriebssystem ein temporäres Verzeichnis zu erhalten. Dies kann mithilfe von "fopen" und dem speziellen Präfix "tmp" erreicht werden, wie z.B. "tmpfile.txt".

Bei der Verwendung von temporären Dateien ist es wichtig, auf die Performance und Sicherheit zu achten. Es ist ratsam, die temporäre Datei so schnell wie möglich zu löschen, um Speicherlecks zu vermeiden. Es ist auch wichtig, sicherzustellen, dass die temporäre Datei nicht von anderen Programmen manipuliert oder gelesen werden kann.

# Siehe auch
Weitere nützliche Ressourcen zum Erstellen von temporären Dateien in C++:

- [C++ Reference – tmpfile](https://en.cppreference.com/w/cpp/io/c/tmpfile)
- [C++ Reference - tmpnam](https://en.cppreference.com/w/cpp/io/c/tmpnam)
- [C++ Reference - tempnam](https://en.cppreference.com/w/cpp/io/c/tempnam)