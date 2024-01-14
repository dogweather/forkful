---
title:    "C++: Überprüfen, ob ein Verzeichnis existiert"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

In der Welt des Programmierens gibt es viele Aufgaben, die uns dazu zwingen, bestimmte Bedingungen zu überprüfen, um sicherzustellen, dass unser Code reibungslos funktioniert. Eine dieser Aufgaben ist das Überprüfen, ob ein Verzeichnis existiert. Obwohl es auf den ersten Blick wie eine einfache Aufgabe erscheint, ist es wichtig, sie richtig zu verstehen und zu implementieren, um zukünftige Fehler zu vermeiden.

## Wie

Um zu überprüfen, ob ein Verzeichnis existiert, müssen wir zuerst die Header-Datei `fstream` inkludieren. Diese Header-Datei enthält die Funktion `opendir()`, die uns dabei helfen wird, das Verzeichnis zu öffnen. Wir müssen auch die Funktion `closedir()` verwenden, um das Verzeichnis nach der Überprüfung wieder zu schließen.

```
#include <fstream>

bool checkDirectory(std::string path){
    DIR *dir = opendir(path.c_str());
    if (dir){
        closedir(dir);
        return true;
    }
    else{
        return false;
    }
}
```

In diesem Beispiel verwenden wir die Funktion `opendir()` und übergeben ihr den Pfad des Verzeichnisses als Argument. Wenn das Verzeichnis erfolgreich geöffnet werden kann, wird die Funktion `opendir()` eine gültige Zeiger-Adresse für das Verzeichnis zurückgeben. Andernfalls wird sie `NULL` zurückgeben. Wir überprüfen diese Rückgabe und schließen das Verzeichnis entsprechend.

Um die Funktion verwenden zu können, müssen wir den Pfad des Verzeichnisses als Argument übergeben. Hier sind zwei Beispiele:

```
std::string path = "/usr/bin";
bool result = checkDirectory(path);
// Ergebnis: result = true

std::string path2 = "/usr/fake";
bool result = checkDirectory(path2);
// Ergebnis: result = false
```

In diesem Beispiel übergeben wir verschiedene Pfade an unsere Funktion `checkDirectory()` und erhalten entsprechende Ergebnisse zurück. Wir können diese Funktion auch innerhalb unserer Programme verwenden, um bestimmte Bedingungen abzufangen und unseren Code daran anzupassen.

## Deep Dive

Während die Funktion `opendir()` eine wirklich hilfreiche Funktion ist, gibt es noch einige Dinge, die wir beachten müssen. Zum Beispiel müssen wir sicherstellen, dass der übergebene Pfad gültig ist und dass wir die Funktion `closedir()` nach der Verwendung von `opendir()` aufrufen.

Außerdem gibt es noch eine andere Funktion namens `access()`, die uns dabei helfen kann, zu überprüfen, ob ein Verzeichnis existiert. Diese Funktion ist jedoch ein bisschen anders als `opendir()` und benötigt verschiedene Berechtigungen, wie z.B. Lese- und Schreibrechte, um erfolgreich zu sein.

## Siehe auch

- [Verzeichnis erstellen in C++](https://www.geeksforgeeks.org/create-directoryfolder-cc-program/)
- [Prüfen, ob Datei existiert in C++](https://www.geeksforgeeks.org/check-if-a-file-exists-in-c-cpp/)
- [FSM-Programmierer-Handbuch](https://www.fsm.com/programmiererhandbuch/verzeichnisprogrammierung)