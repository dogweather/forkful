---
title:                "C++: Schreiben auf den Standardfehler"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie jemals eine Fehlermeldung in der Kommandozeile gesehen haben, haben Sie auch schon einmal gesehen, wie eine Ausgabe auf den Standardfehlerstream geschrieben wird. Aber warum sollte man sich damit beschäftigen, Dinge auf den Standardfehlerstream zu schreiben? In diesem Artikel werde ich Ihnen genau das erklären.

## Wie es gemacht wird
Um auf den Standardfehlerstream zu schreiben, müssen Sie die Standardbibliothek für Ein- und Ausgabe in C++ verwenden. Der folgende Code zeigt, wie Sie einen Text auf den Standardfehlerstream schreiben können:
```C++
#include <iostream>
int main() {
  std::cerr << "Dieser Text wird auf den Standardfehlerstream geschrieben.";
  return 0;
}
```
Die Ausgabe sieht dann folgendermaßen aus:
```
Dieser Text wird auf den Standardfehlerstream geschrieben.
```
Wie Sie sehen, wird die Ausgabe auf der Konsole ausgegeben, aber sie wird in roter Schrift angezeigt, um anzuzeigen, dass es sich um eine Fehlermeldung handelt. Dies kann nützlich sein, um Fehler in einem Programm anzuzeigen oder um die Durchführung des Programms zu verfolgen.

### Weitere Beispiele

Sie können auch Variablen oder Berechnungen auf den Standardfehlerstream schreiben:
```C++
#include <iostream>
int main() {
  int zahl = 5;
  std::cerr << "Die Zahl ist: " << zahl << ".";
  return 0;
}
```
Die Ausgabe lautet dann:
```
Die Zahl ist: 5.
```
Sie können auch andere Datentypen auf den Standardfehlerstream schreiben und sogar mehrere Ausgaben hintereinander vornehmen.

## Tiefergehende Informationen

Der ursprüngliche Zweck des Standardfehlerstreams war es, Fehlermeldungen auszugeben. Es ist jedoch auch nützlich, um Informationen oder Warnungen auszugeben, wenn Sie Ihr Programm ausführen. Sie können sogar eine Kombination aus Ausgaben auf den Standardfehlerstream und den Standardausgabestream verwenden, um verschiedene Arten von Informationen anzuzeigen.

Eine andere nützliche Funktion des Standardfehlerstreams ist das Duplizieren auf die Standardausgabe. Dies kann hilfreich sein, wenn Sie auf eine Datei schreiben möchten, aber auch die Ausgabe auf dem Bildschirm sehen möchten.

Insgesamt bietet der Standardfehlerstream eine einfache und nützliche Möglichkeit, Informationen und Fehlermeldungen in C++ zu verarbeiten.

## Siehe auch

- [C++ Tutorial: Ein- und Ausgabe](https://www.cplusplus.com/doc/tutorial/basic_io/) 
- [C++ Referenz: std::cerr](https://www.cplusplus.com/reference/iostream/cerr/) 
- [GeeksforGeeks: Standard Error Stream in C++](https://www.geeksforgeeks.org/standard-error-stream-c/)