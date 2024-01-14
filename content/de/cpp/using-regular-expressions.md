---
title:                "C++: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie jemals versucht haben, bestimmte Muster in einem Text zu suchen oder zu ersetzen, haben Sie wahrscheinlich schon von regulären Ausdrücken (oder Regex) gehört. Mit regulären Ausdrücken können Sie Text in einem Dokument effizient durchsuchen und bearbeiten, ohne dass Sie jede Zeile manuell überprüfen müssen. Sie sind unglaublich nützlich für Entwickler, die mit großen Mengen an Daten arbeiten, und können Zeit und Mühe sparen. Lassen Sie uns einen Blick darauf werfen, wie man reguläre Ausdrücke in C++ verwenden kann.

## Wie geht das

Es gibt einige grundlegende Schritte, die Sie befolgen können, um reguläre Ausdrücke in C++ zu nutzen. Zunächst müssen Sie die Bibliothek "regex" in Ihr Programm einbinden. Dann können Sie einen regulären Ausdruck als String erstellen und ihn mit der Funktion "std::regex_match" auf Übereinstimmungen prüfen. Alternativ können Sie auch die Funktion "std::regex_search" verwenden, um alle Übereinstimmungen in einem Text zu finden.

Lassen Sie uns ein Beispiel betrachten, in dem wir nach einer bestimmten Telefonnummer in einem Text suchen. Zuerst müssen wir die Bibliothek einbinden:

```
#include <regex>
```

Dann erstellen wir unseren regulären Ausdruck und prüfen, ob er in unserem Text vorkommt:

```
std::regex reg("\\d{3}-\\d{3}-\\d{4}");
std::string text = "Meine Telefonnummer ist 123-456-7890";
if (std::regex_search(text, reg)){
  std::cout << "Eine Telefonnummer wurde gefunden!" << std::endl;
}
```

In diesem Beispiel verwenden wir den regulären Ausdruck "\d{3}-\d{3}-\d{4}", um nach der typischen amerikanischen Telefonnummer-Formatierung zu suchen. Die Ausgabe wird "Eine Telefonnummer wurde gefunden!" sein.

## Tiefergehende Informationen

Es gibt eine Vielzahl von Möglichkeiten und Funktionen, die man beim Arbeiten mit regulären Ausdrücken in C++ verwenden kann. Hier sind einige nützliche Ressourcen, die Ihnen helfen können, tiefer in dieses Thema einzutauchen:

- [C++ Referenz zu regulären Ausdrücken](https://en.cppreference.com/w/cpp/regex)
- [Regex Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)
- [Tutorial zu regulären Ausdrücken in C++](https://www.tutorialspoint.com/cpp_standard_library/cpp_regex_library.htm)

## Siehe auch

- [Wie man reguläre Ausdrücke in Python verwendet](https://www.example.com/article/py-regex)
- [Einführung in die Verwendung von regulären Ausdrücken in Java](https://www.example.com/article/java-regex)