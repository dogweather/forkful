---
title:                "Befehlszeilenargumente lesen"
html_title:           "Arduino: Befehlszeilenargumente lesen"
simple_title:         "Befehlszeilenargumente lesen"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Lesen von Kommandozeilenargumenten in C++ bezeichnet den Prozess der Annahme von Werten von der Kommandozeile während der Ausführung einer Anwendung. Diese Technik ermöglicht es Programmierern, flexibler auf Benutzereingaben zu reagieren und interaktive Programme zu erstellen.
  
## Wie zu:
In C++ können Sie Kommandozeilenargumente durch zwei Funktionsparameter im Hauptteil eines Programms (in der Funktion `main()`) erfassen: `int main(int argc, char *argv[])`. Hier ist `argc` eine Integer-Variable, die die Anzahl der über die Kommandozeile übergebenen Argumente speichert, während `argv` ein Zeiger auf ein Zeichenarray ist, der die eigentlichen Argumente enthält. Hier ist ein einfaches Beispiel:

```C++
#include <iostream>

int main(int argc, char *argv[]) {
    std::cout << "Programmname: " << argv[0] << std::endl;
    for (int i = 1; i < argc; ++i) {
      std::cout << "Argument " << i << ": " << argv[i] << std::endl;
    }
    return 0;
}
```

Wenn Sie dieses Programm als `myProgram` kompilieren und dann z.B. mit `./myProgram GutenTag` ausführen, erhalten Sie folgende Ausgabe:

```
Programmname: ./myProgram
Argument 1: GutenTag
```

## Tiefer Tauchen
Historisch gesehen ist das Lesen von Kommandozeilenargumenten ein Standardverfahren, das sowohl in UNIX- als auch in Windows-Umgebungen weit verbreitet ist. Es gibt jedoch Alternativen, insbesondere für komplexere Anwendungen. Dazu gehören die Einbettung von Argumenten in Dateien oder das Lesen von Eingaben von Standard-Eingaben oder GUI-Formularen.

Für den Fall der C++-Programmierung verweisen `argc` und `argv` auf Daten, die vom Umgebungs-Betriebssystem zur Verfügung gestellt werden. Ihr genauer Speicherort und Inhalt können je nach System und verwendeter Sprachimplementierung variieren.

## Siehe Auch
Für weiterführende Informationen und erweiterte Nutzungsszenarien besuchen Sie bitte:
* [C++ Dokumentation für Kommandozeilenargumente](http://www.cplusplus.com/articles/DEN36Up4/)
* [Stackoverflow Diskussionen zu C++ Kommandozeilenargumenten](https://stackoverflow.com/questions/3024197/what-does-int-argc-char-argv-mean)
* [Weiterführende Lektüre mit Beispielen](https://www.learncpp.com/cpp-tutorial/command-line-arguments/)