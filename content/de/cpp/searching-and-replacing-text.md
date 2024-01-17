---
title:                "Suchen und Ersetzen von Text"
html_title:           "C++: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was ist es und warum?

Suchen und Ersetzen von Text ist ein häufiger Teil der Programmierung, bei dem bestimmte Zeichenketten in einem Code durch andere ersetzt werden. Programmierer tun dies, um schnell und effizient Änderungen oder Korrekturen im Code durchzuführen.

## Anleitung:

### Beispiel 1:

```C++
#include <iostream>

using namespace std;

int main() {
  // erstellen eines Strings mit dem Wort "Hallo"
  string text = "Hallo";
  
  // Suchen und Ersetzen von "Hallo" mit "Guten Tag"
  text.replace(0,5,"Guten Tag");
  
  // Ausgabe des resultierenden Strings
  cout << text << endl;
  
  return 0;
}
```

#### Ausgabe:
```
Guten Tag
```

### Beispiel 2:

```C++
#include <iostream>

using namespace std;

int main() {
  // erstellen einer Zeichenkette mit dem Wort "Welt"
  string text = "Hallo Welt";
  
  // Suchen und Ersetzen von "Welt" mit "Mars"
  text.replace(6,4,"Mars");
  
  // Ausgabe des resultierenden Strings
  cout << text << endl;
  
  return 0;
}
```

#### Ausgabe:
```
Hallo Mars
```

## Tief tauchen:

Suchen und Ersetzen von Text ist keine neue Idee, da es schon in frühen Textverarbeitungsprogrammen verwendet wurde. Auch in der modernen Programmierung wird es häufig verwendet, um Codeeffizienz und -wartbarkeit zu verbessern.

Alternativen zu dieser Methode sind z.B. reguläre Ausdrücke, welche jedoch eine komplexere Syntax aufweisen. In der Implementierung werden Such- und Ersetzungsalgorithmen verwendet, um effizient durch den Code zu navigieren und die gewünschten Änderungen vorzunehmen.

## Siehe auch:

- [Einführung in C++ Programmierung](https://de.wikibooks.org/wiki/C%2B%2B-Programmierung:_Einf%C3%BChrung)
- [Effektives Suchen und Ersetzen von Text in C++](https://www.codeproject.com/Articles/20091/Effective-C-Text-Search-and-Replace)
- [Reguläre Ausdrücke in C++](https://www.codeproject.com/Articles/5283763/Basic-Regular-Expressions-in-Cplusplus)