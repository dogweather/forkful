---
title:                "Einsatz von regulären Ausdrücken"
html_title:           "Bash: Einsatz von regulären Ausdrücken"
simple_title:         "Einsatz von regulären Ausdrücken"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke sind Muster zur Textsuche und -manipulation. Programmierer nutzen sie, um Text effizient zu durchsuchen, zu überprüfen oder zu ändern.

## How to:
C bietet keine eingebaute Unterstützung für reguläre Ausdrücke, also verwenden wir die `regex.h`-Bibliothek. Hier ein einfaches Beispiel zum Abgleich eines Musters:

```C
#include <stdio.h>
#include <regex.h>

int main() {
    regex_t regex;
    int return_value;
    char * pattern = "^[a-zA-Z]+[0-9]*";
    char * test_string = "Hallo123";

    // Kompiliere regulären Ausdruck
    return_value = regcomp(&regex, pattern, 0);

    // Prüfe, ob das Muster passt
    return_value = regexec(&regex, test_string, 0, NULL, 0);

    if (return_value == 0) {
        printf("Das Muster passt!\n");
    } else {
        printf("Keine Übereinstimmung.\n");
    }

    // Gib Speicher frei
    regfree(&regex);
    return 0;
}
```

Ausgabe:
```
Das Muster passt!
```

## Deep Dive
Reguläre Ausdrücke kamen in den 1950ern auf; sie wurden in der Informatik durch Perl und Unix-Tools wie `sed` und `grep` populär. Heute fast in allen Sprachen vorhanden, bieten sie mächtige Werkzeuge für Textverarbeitung. Alternativen zu regulären Ausdrücken sind Stringvergleiche, Parser und Textverarbeitungsbibliotheken, aber die bieten meist weniger Flexibilität. `regex.h` ist Teil der POSIX C-API; sie muss explizit inkludiert und mittels der Funktion `regcomp` kompiliert werden.

## See Also
- [RegExr](https://regexr.com/): Reguläre Ausdrücke online testen.
- [GNU Regex Dokumentation](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html): Detaillierte Info zu `regex.h`.
- [Online C Compiler](https://www.onlinegdb.com/online_c_compiler): Zum Ausprobieren von C-Code im Browser.