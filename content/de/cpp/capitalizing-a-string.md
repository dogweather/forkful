---
title:                "Einen String großschreiben"
html_title:           "C++: Einen String großschreiben"
simple_title:         "Einen String großschreiben"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Großschreiben einer Zeichenkette bedeutet, jede Kleinbuchstaben in eine Großbuchstaben zu verwandeln. Programmierer machen es, um Text bereinigungen oder zum gruppieren ähnlicher Gegenstände zu ermöglichen.

## So geht's:
```C++
#include<iostream>
#include<algorithm>
#include<string>

using namespace std;

string to_upper(string s){
   transform(s.begin(), s.end(), s.begin(), ::toupper);
   return s;
}

int main() {
   string my_str = "hallo, Welt!";
   cout << to_upper(my_str) << endl; // Gibt "HALLO, WELT!" aus.
   return 0;
}
```
Ein typischer Ausdruck wäre:
```
HALLO, WELT!
```
## Tiefere Einblicke
Historisch gesehen stammt das Konzept des Großschreibens einer Zeichenkette aus der Zeit vor Computern, als es verwendet wurde, um Wichtigkeit oder Betonung hervorzuheben. In Programmierung gibt es viele Alternativ-Methoden, darunter ASCII-Konvertierung oder andere eingebaute Funktionen in verschiedenen Sprachen. In C++ verwenden wir die Transform-Funktion der STL. Sie nimmt vier Parameter: die Anfangs- und Endpunkte des Eingabebereichs, den Anfangspunkt des Ausgabebereichs und die zu verwendende Operation (in diesem Fall ::toupper).

## Siehe auch
Für mehr Information, bitte besuchen Sie:
- [C++ String transform()](https://www.cplusplus.com/reference/algorithm/transform/)
- [ASCII Table](http://www.asciitable.com/) 
- [C++ String-Handling](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)