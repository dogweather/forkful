---
title:                "Eine Zeichenfolge interpolieren"
html_title:           "C++: Eine Zeichenfolge interpolieren"
simple_title:         "Eine Zeichenfolge interpolieren"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
"Ein String ist einfach ein Stück Text, aber manchmal brauchen wir, um sie in unsere Programme einzubinden. Das ist, wo das Interpolieren von Strings kommt in. Es ist eine Möglichkeit, Variablen und andere Werte in einen String zu integrieren, so dass wir eine dynamische und anpassbare Ausgabe erhalten. Programmierer nutzen dies, um ihre Programme interaktiver und flexibler zu gestalten."

## Wie geht's?
"Die Syntax für die String-Interpolation in C ++ ist einfach. Wir verwenden das ```"$ {Variable}"``` Zeichen, um anzuzeigen, dass wir einen Wert in einen String einfügen möchten. Sehen wir uns ein Beispiel an:"

```C++
#include <iostream>

int main()
{
  int num = 5;
  std::string text = "Der Wert von num ist: ${num}";
  std::cout << text << std::endl;
  return 0;
}
```

Das Ergebnis wird sein: "Der Wert von num ist: 5". So einfach ist das!

## Tiefer Tauchen
Das Konzept des String-Interpolierens existiert schon seit vielen Jahren und wird in verschiedenen Programmiersprachen verwendet. Zum Beispiel haben Python und Ruby auch ihre Versionen der String-Interpolation. Eine Alternative zum Interpolieren von Strings ist die Verwendung von String-Konkatenation, bei der mehrere Strings miteinander verbunden werden.

In Bezug auf die Implementierung von String-Interpolation in C ++ gibt es verschiedene Ansätze, aber die gängigste Methode ist die Verwendung von der Standardbibliotheksfunktion ```std::format```. Diese Funktion bietet auch mehrere Formatierungsoptionen für den String, wie z.B. die Anzahl der Nachkommastellen bei Fließkommazahlen.

## Siehe auch
Wenn du mehr über String-Interpolation erfahren möchtest, schau dir diese Links an:

- [C ++ String-Interpolation auf cppreference.com](https://en.cppreference.com/w/cpp/language/string_literal)
- [Python String-Interpolation Tutorial](https://realpython.com/python-string-interpolation/)
- [Ruby String-Interpolation auf ruby-lang.org](https://ruby-doc.org/core-2.6.3/doc/syntax/literals_rdoc.html#label-Here+Documents)