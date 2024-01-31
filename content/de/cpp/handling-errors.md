---
title:                "Fehlerbehandlung"
date:                  2024-01-26T00:49:32.716273-07:00
model:                 gpt-4-1106-preview
simple_title:         "Fehlerbehandlung"

category:             "C++"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/handling-errors.md"
---

{{< edit_this_page >}}

## Was & Warum?
Fehlerbehandlung bedeutet, für den Fall zu planen, dass etwas schiefgeht. Sie ist wichtig, weil sie Abstürze verhindert und Ihre Software robust und benutzerfreundlich macht.

## Wie geht das:
Hier ist ein grundlegender try-catch-Block, um eine Ausnahme zu behandeln:

```cpp
#include <iostream>
#include <stdexcept>

int main() {
    try {
        throw std::runtime_error("Hoppla! Etwas ist schiefgelaufen.");
    } catch (const std::exception& e) {
        std::cerr << "Fehler: " << e.what() << std::endl;
    }
    return 0;
}
```

Beispielausgabe:
```
Fehler: Hoppla! Etwas ist schiefgelaufen.
```

## Vertiefung
C++ verfügt seit seinen Anfängen über Fehlerbehandlung. Die grundlegendste Form war die Überprüfung von Rückgabewerten. Wenn Sie schon länger dabei sind, erinnern Sie sich an die Zeit vor dem Standard: C mit Klassen und manueller Fehlerüberprüfung.

Dann kamen mit C++ Ausnahmen, die uns eine strukturierte Möglichkeit bieten, mit unerwarteten Problemen umzugehen. Eine Ausnahme wird mit `throw` ausgelöst und mit `try/catch` gefangen.

Es treten häufig zwei Arten von Fehlern auf: Logische Fehler, wie eine falsche Berechnung, und Laufzeitfehler, wie der Zugriff auf eine ungültige Speicheradresse. Ausnahmen sind ideal für Laufzeitfehler. Bei logischen Fehlern ist es oft besser, Behauptungen (Assertions) oder Fehlercodes zu verwenden.

Es gibt eine anhaltende Debatte über Ausnahmen versus Fehlercodes. Ausnahmen können langsamer sein und können zu komplexen Kontrollflüssen führen. Fehlercodes sind zwar schneller, können aber den Code unübersichtlich und schwerer wartbar machen. Es ist ein Abwägen, daher ist die Kenntnis Ihres Anwendungsfalls entscheidend.

C++17 führte `std::optional` und `std::variant` ein, welche Alternativen zu Ausnahmen sind. Sie sind nützlich für Funktionen, die möglicherweise ein gültiges Ergebnis liefern oder auch nicht.

Ausnahmesicherheit kann ein weiteres Kopfzerbrechen bereiten. Es geht um die Garantien, die Ihr Code trotz Ausnahmen bietet. Es gibt drei Stufen: grundlegend, stark und nothrow. Je mehr Garantien, desto komplexer könnte Ihr Code sein.

Abschließende Gedanken - Fehlerbehandlung ist genauso viel Kunst wie Wissenschaft. Sie prägt, wie Ihre Anwendung in der freien Wildbahn überlebt. Setzen Sie Ausnahmen nicht übermäßig ein. Ziel sollte lesbarer, wartbarer Code sein.

## Siehe auch
- [cppreference zur Ausnahmebehandlung](https://en.cppreference.com/w/cpp/language/exceptions)
- [Bjarne Stroustrups Meinung zur Fehlerbehandlung](http://www.stroustrup.com/except.pdf)
- [C++-Kernrichtlinien zu Ausnahmen](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Re-exceptions)
