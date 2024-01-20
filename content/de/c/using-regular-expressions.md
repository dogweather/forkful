---
title:                "Reguläre Ausdrücke verwenden"
html_title:           "Bash: Reguläre Ausdrücke verwenden"
simple_title:         "Reguläre Ausdrücke verwenden"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?

Reguläre Ausdrücke (regular expressions) sind Muster, die zur Überprüfung, Suche oder Manipulation von Text verwendet werden. Programmierer nutzen sie, um effizient und präzise Daten zu durchforsten und zu manipulieren.

## So geht's:

Mit POSIX-C Bibliothek kannst du reguläre Ausdrücke benutzen. Hier sind ein einfaches Beispiel:

```C
#include <regex.h> 
#include <stdio.h>

int main() 
{ 
    regex_t regex;
    int result;
    char txt[] = "Erstellen Sie reguläre Ausdrücke in C";
    result = regcomp(&regex, "C", 0);
    result = regexec(&regex, txt, 0, NULL, 0);
    if (result == 0) {
        printf("'C' gefunden.\n");
    } else if (result == REG_NOMATCH) {
        printf("'C' nicht gefunden.\n");
    }  
    regfree(&regex);
    return 0; 
} 
```

Dieses Programm prüft, ob der Text "C" in der angegebenen Zeichenkette vorkommt. Wenn das der Fall ist, wird "C gefunden" ausgegeben.

## Vertiefung

(1) Im Jahr 1956 hat der Mathematiker Stephen Cole Kleene die regulären Ausdrücke erstellt.
(2) Alternativen zu regulären Ausdrücken sind zum Beispiel der Einsatz von `strstr()` für einfache Suchanfragen in C.
(3) Die Implementierung von regulären Ausdrücken in C erfolgt über die Bibliotheksfunktionen `regcomp()` zum Kompilieren des Ausdrucks und `regexec()` zum Ausführen desselben.

## Weiterführende Informationen

[Regular Expressions in POSIX](https://pubs.opengroup.org/onlinepubs/7908799/xbd/re.html)
[C Library - <regex.h>](https://www.tutorialspoint.com/c_standard_library/regex_h.htm)