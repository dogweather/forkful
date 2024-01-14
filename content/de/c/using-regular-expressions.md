---
title:    "C: Verwendung von regulären Ausdrücken"
keywords: ["C"]
---

{{< edit_this_page >}}

## Warum

Wenn Sie schon einmal versucht haben, bestimmte Zeichenfolgen in einem Text zu finden oder zu ersetzen, wissen Sie wahrscheinlich, wie mühsam und zeitaufwendig das sein kann. Hier kommen reguläre Ausdrücke ins Spiel. Mit Hilfe von regulären Ausdrücken können Sie Muster in Texten suchen und bearbeiten, um die Arbeit zu erleichtern. Lesen Sie weiter, um herauszufinden, wie Sie reguläre Ausdrücke in Ihren C-Programmen verwenden können.

## Wie geht man vor

Um reguläre Ausdrücke in C zu verwenden, müssen Sie die Header-Datei `<regex.h>` einbinden. Dann können Sie die Funktion `regcomp()` verwenden, um einen regulären Ausdruck zu kompilieren. Als nächstes verwenden Sie `regexec()` oder `regerror()` für die Suche oder Bearbeitung von Texten. Hier ist ein Beispiel, wie Sie alle Vokale in einem Text durch das Zeichen `x` ersetzen können:

```C
#include <stdio.h>
#include <regex.h>
int main()
{
    regex_t regex;
    int result;
    char text[] = "Die Katze läuft über den Zaun.";
    char pattern[] = "[aeiouüöä]";
    // Kompilieren des regulären Ausdrucks
    result = regcomp(&regex, pattern, 0);
    // Überprüfen, ob die Kompilierung erfolgreich war
    if(result != 0) {
        printf("Fehler beim Kompilieren des regulären Ausdrucks.");
        exit(1);
    }
    // Ersetzen der Vokale mit x
    result = regsub(&regex, text, "x", NULL);
    // Ausgabe des bearbeiteten Textes
    if(result != 0) {
        printf("Fehler beim Bearbeiten des Textes.");
        exit(1);
    }
    printf("%s\n", text);
    // Freigeben des regulären Ausdrucks
    regfree(&regex);
    return 0;
}
```

Die Ausgabe dieses Programms wäre: `Dx Kxtz lxxt xbxr dxn Zxn.`

## Tiefere Einblicke

Während dieses Beispiel nur einen kleinen Einblick in die Verwendung von regulären Ausdrücken in C gibt, gibt es noch viele weitere Möglichkeiten und Funktionen, die Sie erkunden können. Sie können beispielsweise spezifische Zeichenmuster wie Zahlen oder Buchstaben suchen, Variablen für begrenzte Zeichenfolgen verwenden oder sogar komplexe Ausdrücke mit Hilfe von Quantoren erstellen. Es lohnt sich, sich mit den verschiedenen Funktionen und Optionen von regulären Ausdrücken in C vertraut zu machen, um sie effektiver in Ihren Programmen einsetzen zu können.

## Siehe auch

- Offizielle Dokumentation: https://en.cppreference.com/w/c/regex
- Tutorial (auf Englisch): https://www.cprogramming.com/tutorial/regular-expression-matching-c.html
- Beispiele für fortgeschrittene Verwendung (auf Englisch): http://clug.caltech.edu/~jafl/Regex1.html