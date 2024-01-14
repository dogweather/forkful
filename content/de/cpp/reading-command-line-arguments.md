---
title:    "C++: Lesen von Befehlszeilenargumenten"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Warum
Es gibt viele Gründe, warum man sich mit dem Lesen von Befehlszeilenargumenten in C++ beschäftigen möchte. Möglicherweise möchten Sie ein Programm schreiben, das verschiedenen Eingaben basierend auf verschiedenen Ausgangsparametern ausführen kann. Oder vielleicht möchten Sie ein interaktives Tool erstellen, das bestimmte Aktionen basierend auf vom Benutzer eingegebenen Befehlen ausführt. Egal aus welchem Grund, das Lesen von Befehlszeilenargumenten kann Ihnen dabei helfen, flexiblere, interaktive und benutzerfreundlichere Programme zu erstellen.

## Wie geht das?
Das Lesen von Befehlszeilenargumenten in C++ ist relativ einfach. Zunächst müssen Sie die Headerdatei "iostream" und die Headerdatei "string" einbinden, um die Funktionen "cout" und "cin" zu verwenden. Dann können Sie die Funktion "main" wie folgt deklarieren:

```
#include <iostream>
#include <string>

int main(int argc, char *argv[]){
    // Code, um Befehlszeilenargumente zu lesen
}
```

Die Funktion "main" hat zwei Parameter: "argc" und "argv". "argc" gibt die Anzahl der Befehlszeilenargumente an und "argv" ist ein Array, das die Befehlszeilenargumente enthält. Zum Beispiel, wenn Sie Ihr Programm mit dem Befehl "meinProgramm.exe 123 test" ausführen, wäre "argc" 3 und "argv" ein Array mit den Werten "123" und "test". Sie können dann auf diese Werte zugreifen, indem Sie auf die entsprechenden Array-Indizes zugreifen (z.B. argv[0] für "123" und argv[1] für "test").

Im Folgenden finden Sie ein Beispiel, das alle Befehlszeilenargumente ausgibt:

```
#include <iostream>
#include <string>

int main(int argc, char *argv[]){
    for(int i = 0; i < argc; i++){
        std::cout << "Argument " << i << ": " << argv[i] << std::endl;
    }
    return 0;
}
```

Die Ausgabe für den Befehl "meinProgramm.exe Hallo Welt" wäre:

```
Argument 0: meinProgramm.exe
Argument 1: Hallo
Argument 2: Welt
```

## Tiefer in die Materie eintauchen
Das Lesen von Befehlszeilenargumenten kann auch komplexer werden, wenn Sie verschiedene Datentypen umwandeln müssen oder wenn Sie argumentbezogene Optionen hinzufügen möchten. In diesem Fall empfehlen wir Ihnen, sich mit der Bibliothek "getopt" auseinanderzusetzen, die speziell für das Parsen von Befehlszeilenargumenten entwickelt wurde.

Sie können auch mehrere Befehlszeilenargumente in einem einzigen Argument zusammenfassen, indem Sie zum Beispiel das "+"-Zeichen als Trennzeichen verwenden: "meinProgramm.exe -u+john+doe". In diesem Fall wäre "argv[1]" "-u+john+doe", und Sie müssten dann die Zeichenkette in ihre einzelnen Teile zerlegen.

## Siehe auch
- [C++-Befehlszeilenargumente im Detail](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [Befehlszeilenargumente mit getopt in C++](https://www.gnu.org/software/libc/manual/html_node/getopt.html)
- [C++ String-Stream](https://www.cplusplus.com/reference/sstream/istringstream/)