---
title:    "C++: Erstellen einer temporären Datei"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Das Erstellen von temporären Dateien ist eine nützliche Programmiertechnik, um vorübergehend Daten zu speichern, die nur für einen begrenzten Zeitraum benötigt werden. Dies kann hilfreich sein, um die Leistung und Effizienz von Programmen zu verbessern, indem Speicherplatz und Ressourcen eingespart werden.

## Wie man ein temporäres File erstellt

Um ein temporäres File in C++ zu erstellen, müssen wir zunächst die Standardbibliothek "fstream" einbinden. Dann können wir mit der Funktion "tmpnam()" einen eindeutigen Dateinamen generieren und eine Datei mit diesem Namen erstellen. Hier ist ein Beispielcode, der dies demonstriert:

```C++
#include <iostream>
#include <fstream>
using namespace std;

int main() {
    // Eindeutigen Dateinamen erstellen
    char tempName[L_tmpnam];
    tmpnam(tempName);
    
    // Datei mit dem erstellten Namen erstellen
    ofstream tempFile(tempName);
    
    // Daten in die temporäre Datei schreiben
    tempFile << "Das ist ein Beispieltext.";
    
    // Datei schließen
    tempFile.close();
    
    // Ausgabe des Dateinamens
    cout << "Der erstellte Dateiname lautet: " << tempName << endl;
    
    return 0;
}
```

Die Ausgabe dieses Codes könnte zum Beispiel "Der erstellte Dateiname lautet: /tmp/tmp-file.tmp" lauten.

## Tiefergehende Informationen

Das oben gezeigte Beispiel verwendet die Funktion "tmpnam()", um einen eindeutigen Dateinamen zu generieren. Eine alternative Möglichkeit ist die Verwendung der Funktion "tmpfile()", die automatisch eine Datei mit einem eindeutigen Namen erstellt und ein File-Handle zurückgibt. Dieses Handle kann dann verwendet werden, um auf die Datei zuzugreifen.

Eine andere wichtige Sache, die beim Erstellen von temporären Dateien zu beachten ist, ist die Sicherheit. Da temporäre Dateien oft für kurze Zeit verwendet werden und nicht dauerhaft gespeichert werden, ist es wichtig sicherzustellen, dass sie nach der Verwendung gelöscht werden. Hier ist ein Beispielcode, der dies demonstriert:

```C++
#include <iostream>
#include <fstream>
#include <cstdio>
using namespace std;

int main() {
    // Eindeutigen Dateinamen erstellen
    char tempName[L_tmpnam];
    tmpnam(tempName);
    
    // Datei mit dem erstellten Namen erstellen
    ofstream tempFile(tempName);
    
    // Daten in die temporäre Datei schreiben
    tempFile << "Das ist ein Beispieltext.";
    
    // Datei schließen
    tempFile.close();
    
    // Datei löschen
    remove(tempName);
    
    return 0;
}
```

## Siehe auch

- Weitere Informationen zum Erstellen und Löschen von temporären Dateien in C++: https://www.geeksforgeeks.org/temporary-file-generation-in-c-programming/
- Eine Diskussion über die verschiedenen Methoden zum Erstellen temporärer Dateien: https://stackoverflow.com/questions/1682913/creating-a-temporary-file-in-c
- Ein Beispielprogramm zum Erstellen von temporären Dateien in C++: https://www.programiz.com/cpp-programming/examples/temporary-file