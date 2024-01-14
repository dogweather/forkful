---
title:                "C++: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum
Es gibt viele Gründe, warum man im C++ Programmierumfeld temporäre Dateien erstellen möchte. Zum Beispiel können sie verwendet werden, um temporäre Daten zu speichern, die während der Programmablaufzeit benötigt werden, oder um bestimmte Daten zwischen verschiedenen Programmaufrufen zu teilen. Temporäre Dateien können auch zum Debuggen oder zur Fehlerbehebung dienen.

## So geht's: Erstellen einer temporären Datei in C++
Um eine temporäre Datei in C++ zu erstellen, müssen wir einige wichtige Schritte beachten.

Zunächst müssen wir die erforderlichen Header-Dateien inkludieren. Dazu gehören <fstream> für das Lesen und Schreiben von Dateien und <cstdlib> für die Verwendung von temporären Dateinamen.

Als nächstes müssen wir einen temporären Dateinamen generieren. Dazu können wir die Funktion ```tempnam()``` aus der <cstdlib>-Bibliothek verwenden. Diese Funktion erstellt einen temporären Dateinamen, der mit dem Präfix "tmp" beginnt und im Standardverzeichnis für temporäre Dateien gespeichert wird.

Nachdem wir den Dateinamen generiert haben, können wir anschließend die temporäre Datei erstellen. Wir verwenden dazu die Funktion ```std::ofstream``` zum Erstellen der Datei und ```open()``` zum Öffnen der Datei mit dem generierten Dateinamen.

Im folgenden Code-Beispiel erstellen wir eine temporäre Datei mit dem Namen "tmpFile.txt" und schreiben den Text "Dies ist eine temporäre Datei!" in die Datei:

```C++
#include <fstream>
#include <cstdlib>

int main()
{
    // Generieren des temporären Dateinamens
    char* tmpFileName = tempnam(NULL, "tmp");

    // Erstellen und Öffnen der temporären Datei
    std::ofstream tmpFile;
    tmpFile.open(tmpFileName);

    // Schreiben in die Datei
    tmpFile << "Dies ist eine temporäre Datei!";

    // Schließen und Löschen der Datei
    tmpFile.close();
    remove(tmpFileName);

    return 0;
}
```

Die obigen Schritte erstellen eine temporäre Datei und schreiben den Text in die Datei. Zum Abschluss schließen wir die Datei und löschen sie, um die Ressourcen freizugeben.

## Tiefergehende Informationen
Beim Erstellen einer temporären Datei gibt es viele Aspekte zu beachten. Zum Beispiel können wir die Funktion ```std::tmpnam()``` statt ```tempnam()``` verwenden, um einen temporären Dateinamen zu generieren. Diese Funktion ist jedoch nicht portabel und kann auf einige Systemen nicht verfügbar sein.

Auch die Verwendung der Funktion ```remove()``` zum Löschen der temporären Datei kann problematisch sein, da es nicht garantiert ist, dass die Datei tatsächlich gelöscht wird. In diesem Fall ist es besser, eine andere Methode zum Löschen der Datei zu verwenden, wie z.B. die Funktion ```std::remove()``` aus der <cstdio>-Bibliothek.

Der Umgang mit temporären Dateien erfordert auch ein gutes Verständnis für die Dateioperationen in C++. Es ist wichtig, sicherzustellen, dass die Datei korrekt geöffnet, geschlossen und gelöscht wird, um mögliche Fehler zu vermeiden.

## Siehe auch
- [C++ Datei Operationen](https://www.programiz.com/cpp-programming/library-function/cstdio)
- [C++ Datei Eingabe/Ausgabe](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
- [Erstellen temporärer Dateinamen in C++](https://www.codingame.com/playgrounds/14213/temporary-file-in-c-plus-plus/how-to-create-a-temporary-file-in-c)