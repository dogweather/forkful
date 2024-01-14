---
title:                "C++: Erstellen einer temporären Datei"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Warum

Temporary-Dateien sind ein wichtiger Bestandteil vieler C++-Programme. Sie dienen dazu, temporäre Daten zu speichern, die während der Laufzeit des Programms benötigt werden, aber nicht in der endgültigen Ausgabe enthalten sein sollen. Dies kann aus verschiedenen Gründen geschehen, zum Beispiel um Zwischenergebnisse zu speichern oder um Dateien zu erstellen, die nur für kurze Zeit benötigt werden.

# Wie erstellt man eine temporäre Datei in C++

Um eine temporäre Datei in C++ zu erstellen, gibt es verschiedene Möglichkeiten. Eine einfache Möglichkeit ist die Verwendung der Funktion "tmpnam", die in der Standardbibliothek <cstdio> definiert ist. Sie generiert einen eindeutigen Dateinamen und erstellt die Datei automatisch. Hier ist ein Beispielcode:

```C++ 
#include <cstdio> 
int main() 
{ 
char tempName[L_tmpnam]; 
tmpnam(tempName); 
FILE *fp = fopen(tempName,"w"); 
fprintf(fp,"Dies ist eine temporäre Datei!"); 
fclose(fp); 
} 
```

Dieser Code generiert einen temporären Dateinamen in der Variablen tempName und erstellt dann eine Datei mit diesem Namen. Der Inhalt der Datei wird mit Hilfe der Funktion "fprintf" geschrieben und die Datei anschließend mit "fclose" geschlossen.

Eine andere Möglichkeit ist die Verwendung der Funktion "mkstemp", die in der Bibliothek <cstdlib> definiert ist. Im Gegensatz zu "tmpnam" gibt "mkstemp" auch einen offenen Datei-Deskriptor zurück, der für weitere Aktionen mit der Datei verwendet werden kann. Hier ist ein Beispielcode:

```C++ 
#include <cstdlib> 
int main() 
{ 
char tempName[L_tmpnam]; 
int fd; 
fd = mkstemp(tempName); 

FILE *fp = fdopen(fd,"w"); 
fprintf(fp,"Dies ist eine temporäre Datei!"); 
fclose(fp); 
} 
```

Diese Methode ist etwas komplexer, da sie ein offenes Datei-Handle verwendet. In diesem Fall wird die Datei jedoch nicht automatisch gelöscht, wenn das Programm beendet wird, daher müssen Sie zusätzlichen Code hinzufügen, um die Datei später zu löschen.

# Tiefere Einblicke

Das Erstellen einer temporären Datei mit "tmpnam" oder "mkstemp" ist nur eine von vielen Möglichkeiten. Es gibt noch andere, wie z.B. die Verwendung von Standard-Libraries in verschiedenen Betriebssystemen oder die Verwendung von Bibliotheken von Drittanbietern. Es kann auch sinnvoll sein, zusätzliche Sicherheitsmaßnahmen zu implementieren, um sicherzustellen, dass die temporären Dateien nicht von anderen Prozessen manipuliert werden können.

Das Erstellen von temporären Dateien kann auch in verschiedenen Szenarien nützlich sein, wie z.B. beim Testen von neuen Funktionen oder beim Speichern von temporären Benutzerdaten. Es ist wichtig, nur die Informationen zu speichern, die für das Programm benötigt werden, und die Datei nach Gebrauch zu löschen, um sicherzustellen, dass keine vertraulichen Daten gefährdet werden.

# Siehe auch

- [C++: Dateien und Dateisystem](https://www.cplusplus.com/doc/tutorial/files/)
- [tmpnam-Funktion (cplusplus.com Referenz)](https://www.cplusplus.com/reference/cstdio/tmpnam/)
- [mkstemp-Funktion (cplusplus.com Referenz)](https://www.cplusplus.com/reference/cstdlib/mkstemp/)