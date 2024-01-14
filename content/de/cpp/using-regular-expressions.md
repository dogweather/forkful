---
title:                "C++: Verwendung von Regulären Ausdrücken"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum (Why)

Reguläre Ausdrücke sind ein wichtiges Werkzeug für Programmierer, um Muster in Texten zu finden und zu manipulieren. Sie können hilfreich sein, wenn man beispielsweise Daten aus einer Datei extrahieren oder Benutzereingaben validieren möchte.

## Wie (How To)

Die Verwendung von regulären Ausdrücken in C++ ist relativ einfach. Zunächst müssen Sie das <regex> Header-Datei in Ihr Programm einbinden. Dann können Sie mithilfe der Klasse `regex` ein Muster definieren und mit der `match` Funktion überprüfen, ob eine Zeichenkette diesem Muster entspricht.

Hier ist ein einfaches Beispiel, das überprüft, ob eine E-Mail-Adresse gültig ist:

```C++
#include <iostream>
#include <regex> 

int main() {
    std::string email = "beispiel@beispieldomain.de";
    std::regex regex_pattern("([\\w-]+)@([\\w-]+)(\\.[\\w-]+)+");
    if (std::regex_match(email, regex_pattern)) {
        std::cout << "Gültige E-Mail-Adresse." << std::endl;
    }
    else {
        std::cout << "Ungültige E-Mail-Adresse." << std::endl;
    }
    
    return 0;
}
```

Die Ausgabe dieses Programms wäre "Gültige E-Mail-Adresse."

## Tiefenschärfe (Deep Dive)

Reguläre Ausdrücke bieten viele verschiedene Möglichkeiten, um Texte zu durchsuchen und zu manipulieren. Hier sind einige wichtige Konzepte, die es zu beachten gilt:

- Zeichenklassen: Sie können bestimmte Zeichen spezifizieren, die in einer Zeichenkette vorkommen müssen, z.B. `[a-z]` für Kleinbuchstaben oder `[0-9]` für Zahlen.

- Quantoren: Sie legen fest, wie oft ein vorhergehendes Element in einer Zeichenkette vorkommen muss, z.B. `+` für einmal oder öfter, `*` für beliebig oft oder `?` für einmal oder gar nicht.

- Gruppen: Sie können Teile eines Musters in Gruppen zusammenfassen, um sie später zu referenzieren oder zu extrahieren.

Für eine ausführlichere Anleitung zu regulären Ausdrücken in C++ können Sie die offizielle C++-Referenz des `<regex>`-Headers oder andere Online-Ressourcen wie beispielsweise [diese](https://www.cplusplus.com/reference/regex/regex/) konsultieren.

## Siehe auch (See Also)

- [C++ Referenz zu regulären Ausdrücken](https://www.cplusplus.com/reference/regex/)
- [Leitfaden zu regulären Ausdrücken in C++](https://www.geeksforgeeks.org/regular-expressions-in-c-c/)
- [Howto Regex-Tutorial für C++](https://howtobuildsoftware.com/index.php/how-fb-cpp/cplusplus-regex-tutorial-how-to-use-regular-expression-pattern-matching-library-examples)