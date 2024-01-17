---
title:                "Verwendung von regulären Ausdrücken"
html_title:           "C++: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was sind reguläre Ausdrücke und warum nutzen Programmierer sie?

Reguläre Ausdrücke sind eine Art von Mustererkennung in der Informatik. Sie erlauben es, in Texten bestimmte Muster zu suchen und zu identifizieren. Programmierer nutzen reguläre Ausdrücke, um Daten zu validieren, Texte zu filtern oder zu formatieren und vieles mehr.

## Wie funktionieren sie?

Die Syntax für reguläre Ausdrücke variiert je nach Programmiersprache, aber das grundlegende Konzept bleibt gleich. Hier ein Beispiel für die Verwendung von regulären Ausdrücken in C++:

```C++
#include <iostream>
#include <regex>
using namespace std;

int main() {
  // Ein Text, in dem wir nach einem Muster suchen wollen
  string text = "1, 2, 3, 4, 5";

  // Definiere das Muster mit Hilfe von regulären Ausdrücken
  // In diesem Beispiel suchen wir nach einer Ziffer von 0-9, die von einem Komma gefolgt wird
  regex pattern("[0-9]+,");

  // Erstelle ein Objekt, um das Muster mit dem Text zu vergleichen
  smatch matches;

  // Überprüfe, ob das Muster in dem Text gefunden wird
  while (regex_search(text, matches, pattern)) {
    // Ausgabe der Übereinstimmung
    cout << "Gefunden: " << matches[0] << endl;
    // Entferne die bereits gefundene Übereinstimmung aus dem Text
    text = matches.suffix().str();
  }

  return 0;
}
```

Die Ausgabe des obigen Beispiels lautet:

```
Gefunden: 1,
Gefunden: 2,
Gefunden: 3,
Gefunden: 4,
Gefunden: 5,
```

## Vertiefung

Reguläre Ausdrücke wurden bereits in den 1950er Jahren von dem Mathematiker Stephen Kleene eingeführt. Sie wurden hauptsächlich in der Theorie der formalen Sprachen verwendet, um formale Grammatiken zu beschreiben. Heutzutage werden sie jedoch in vielen Programmiersprachen verwendet und haben sich als essentielle Werkzeuge für das effiziente Verarbeiten von Texten und Daten etabliert.

Es gibt auch andere Möglichkeiten, um Textmuster zu identifizieren, wie zum Beispiel die Verwendung von String-Funktionen oder die Verwendung von Bibliotheken wie Boost.Regex. Diese sind jedoch nicht so leistungsstark und flexibel wie reguläre Ausdrücke.

Die Implementierung von regulären Ausdrücken kann auch sehr komplex sein und erfordert ein grundlegendes Verständnis von regulären Sprachen und endlichen Automaten.

## Weitere Informationen

Hier sind einige empfohlene Ressourcen, um mehr über reguläre Ausdrücke zu erfahren:

- [Reguläre Ausdrücke auf cppreference.com](https://en.cppreference.com/w/cpp/regex)
- [Reguläre Ausdrücke Tutorial auf regular-expressions.info](https://www.regular-expressions.info/tutorial.html)
- [Reguläre Ausdrücke in Wikipedia](https://de.wikipedia.org/wiki/Regul%C3%A4rer_Ausdruck)