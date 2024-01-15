---
title:                "Substrings extrahieren"
html_title:           "C++: Substrings extrahieren"
simple_title:         "Substrings extrahieren"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

# Warum

Wenn du C++ als Programmiersprache verwendest, musst du manchmal bestimmte Teilstrings oder Unterzeichenketten aus einem String extrahieren. Das kann nützlich sein, um beispielsweise bestimmte Informationen aus Nutzereingaben zu erhalten oder um Texte zu manipulieren.

# Wie geht das?

Die Extraktion von Teilstrings in C++ ist recht einfach und kann auf verschiedene Arten erfolgen. Im Folgenden findest du drei Methoden, um Teilstrings in einem String zu extrahieren.

```C++
// Beispiel String
std::string beispiel = "Ich liebe C++ Programmierung";

// Methode 1: std::string::substr()
// Mit substr() können wir Teilstrings anhand von Start- und Endpositionen extrahieren.
// Hier extrahieren wir den Teilstring "C++".
int start_pos = 9; // Die Position, an der der Teilstring beginnt (inklusive)
int length = 3; // Die Länge des Teilstrings
std::string teilstring = beispiel.substr(start_pos, length); // Gibt "C++" zurück

// Methode 2: std::string::find()
// Mit find() können wir eine bestimmte Zeichenfolge in einem String suchen und die Position zurückgeben.
// Hier suchen wir nach dem ersten Vorkommen von "b" und extrahieren den Teilstring bis zum Ende des Strings.
int position = beispiel.find("b"); // Gibt 13 zurück (Position des ersten "b")
std::string teilstring2 = beispiel.substr(position); // Gibt "bung" zurück

// Methode 3: std::string::find_first_of()
// Mit find_first_of() können wir nach einem beliebigen Zeichen aus einer Liste suchen.
// Hier extrahieren wir den Teilstring bis zum ersten Leerzeichen.
int position2 = beispiel.find_first_of(" "); // Gibt 2 zurück (Position des ersten Leerzeichens)
std::string teilstring3 = beispiel.substr(0, position2); // Gibt "Ich" zurück
```

### Ausgabe:

Teilstring: C++ <br>
Teilstring 2: bung <br>
Teilstring 3: Ich

# Tiefere Einblicke

Du hast jetzt gesehen, wie einfach es ist, Teilstrings in C++ zu extrahieren. Aber es gibt noch mehr Möglichkeiten und Methoden, die du ausprobieren kannst, z.B. das Ersetzen von Teilstrings oder die Verwendung von Regulären Ausdrücken mit der Bibliothek <regex>. Experimentiere ein wenig und finde heraus, welche Methode am besten für deine spezifischen Anforderungen geeignet ist.

# See Also

- [std::string::substr() Dokumentation](https://en.cppreference.com/w/cpp/string/basic_string/substr)
- [std::string::find() Dokumentation](https://en.cppreference.com/w/cpp/string/basic_string/find)
- [std::string::find_first_of() Dokumentation](https://en.cppreference.com/w/cpp/string/basic_string/find_first_of)