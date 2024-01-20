---
title:                "Strings verketten"
html_title:           "Bash: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

# Der Umgang mit Zeichenkettenverknüpfung in C++

## Was & Warum?
Die Zeichenkettenverknüpfung ist der Prozess, zwei oder mehr Zeichenketten zu einer einzigen zusammenzuführen. Dies ist nützlich für Aufgaben wie Formatierung von Ausgaben und Datenmanipulation.

## Anleitung: 
Weiter geht’s mit der Praxis. Werfen wir einen Blick auf einige gängige Methoden zur Zeichenkettenverknüpfung in C++.

```C++
// Methode 1: Mit Hilfe des Operators '+'
std::string str1 = "Hallo, ";
std::string str2 = "Welt!";
std::string str3 = str1 + str2;  // "Hallo, Welt!"

// Methode 2: Verwenden der Funktion append()
std::string str4 = "Hallo, ";
str4.append("Welt!");    // str4 ist jetzt "Hallo, Welt!"
```

Ergebnis:

```
Hallo, Welt!
Hallo, Welt!
```

## Vertiefung
Die Verknüpfung von Zeichenketten ist ein altes Konzept und existierte schon lange vor C++. In unterschiedlichen Programmiersprachen gibt es verschiedene Methoden, einige performanter als andere.

Eine Alternative zur Zeichenkettenverknüpfung in C++ ist die Verwendung von `std::stringstream`. Dies kann besonders nützlich sein, wenn Sie viele Zeichenketten aneinanderhängen, da `std::stringstream` effizienter ist als wiederholte Verwendung von `+` oder `append()`.

Es ist wichtig zu wissen, dass bei Verwendung des `+`-Operators eine neue Zeichenkette erstellt wird, wodurch mehr Speicher benötigt wird. Bei der `append()`-Methode wird hingegen die ursprüngliche Zeichenkette verändert, was eine effizientere Arbeitsweise ermöglicht.

```C++
// Methode 3: Verwendung von stringstream
std::stringstream ss;
ss << "Hallo, ";
ss << "Welt!";
std::string str5 = ss.str();   // "Hallo, Welt!"
```

## Siehe Auch
2. [C++ Strings](https://www.w3schools.com/cpp/cpp_strings.asp)