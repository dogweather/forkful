---
title:                "Reguläre Ausdrücke verwenden"
html_title:           "Bash: Reguläre Ausdrücke verwenden"
simple_title:         "Reguläre Ausdrücke verwenden"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Reguläre Ausdrücke in C++: Ein Leitfaden 

## Was & Warum?

Reguläre Ausdrücke (oder Regex) sind Muster, die Entwickler verwenden, um Textzeichenketten zu analysieren und zu manipulieren. Sie sind ein mächtiges Werkzeug, das zum Durchsuchen, Ersetzen und Überprüfen von Text genutzt wird.

## Wie es geht:

Ein einfaches C++ Beispiel, das `regex` und `sregex_iterator` verwendet, um alle Wörter in einem String zu finden.

```C++
#include <iostream>
#include <regex>
#include <string>

int main()
{
    std::string s = "Reguläre Ausdrücke sind wirklich cool!";
    std::regex Wort_regex("(\\S+)");

    auto Worte_beginnen = std::sregex_iterator(s.begin(), s.end(), Wort_regex);
    auto Worte_enden = std::sregex_iterator();

    std::cout << "Es wurden " << std::distance(Worte_beginnen, Worte_enden)
              << " Wörter gefunden.\n";

    for (std::sregex_iterator i = Worte_beginnen; i != Worte_enden; ++i)
    {
        std::smatch match = *i;
        std::string match_str = match.str();
        std::cout << match_str << '\n';
    }

    return 0;
}
```

## Vertiefende Informationen

Reguläre Ausdrücke werden seit den 1950er Jahren verwendet und waren eine Schlüsseltechnologie in frühen Texteditoren und in der Unix-Programmierung. Sie sind zwar mächtig, aber auch verwirrend und schwierig zu beherrschen. Alternativen könnten domänenspezifische Sprachen oder String-Manipulationsbibliotheken sein. Die Implementierung von Regex in C++ erfolgt über die Regex-Bibliothek, die im `<regex>` Header definiert ist.

## Siehe auch

- [C++ Referenz: Reguläre Ausdrücke](http://en.cppreference.com/w/cpp/regex)
- [RegexOne: Interaktive Lektionen](https://regexone.com/)