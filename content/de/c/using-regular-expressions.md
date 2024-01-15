---
title:                "Verwendung von regulären Ausdrücken"
html_title:           "C: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum
Regular Expressions, oder auch Reguläre Ausdrücke genannt, sind ein mächtiges Werkzeug in der Programmierung. Mit ihrer Hilfe können Texte nach bestimmten Mustern durchsucht und verarbeitet werden. Dies kann in vielen Situationen nützlich sein, wie zum Beispiel beim Validieren von Benutzereingaben oder beim Extrahieren von Daten aus großen Textdateien. In diesem Artikel lernen wir, wie man Reguläre Ausdrücke in C nutzen kann, um effektiv mit Texten umzugehen.

## How To
Um Reguläre Ausdrücke in C zu verwenden, müssen wir das Headerfile "regex.h" einbinden. Dies ermöglicht uns die Verwendung von Funktionen wie "regcomp()" und "regexec()", welche uns dabei helfen, Reguläre Ausdrücke zu erstellen und auf Texte anzuwenden.

Um einen Regulären Ausdruck zu erstellen, müssen wir zunächst die gewünschte Mustersequenz in Form eines Strings angeben, zum Beispiel ```"[0-9]+"```, was bedeutet, dass jedes Zeichen von 0 bis 9 mindestens einmal vorkommen muss. Anschließend verwenden wir die Funktion "regcomp()" um den Ausdruck zu kompilieren und in eine spezielle Struktur zu übertragen.

Um nun diesen Regulären Ausdruck auf einen Text anzuwenden, benutzen wir die Funktion "regexec()" und übergeben ihr den kompilierten Ausdruck sowie den zu durchsuchenden Text. Diese Funktion gibt uns ein Ergebnis zurück, welches angibt, ob der Ausdruck im Text gefunden wurde und welche Stellen im Text dazu passen.

Schauen wir uns ein Beispiel an:

```C
#include <regex.h>
#include <stdio.h>

int main(){
    regex_t exp;
    char *pattern = "[0-9]+";
    char *text = "12345 abcde6789";

    if(regcomp(&exp, pattern, 0) == 0){
        int result = regexec(&exp, text, 0, NULL, 0);
        if(result == REG_NOMATCH){
            printf("Keine Übereinstimmung gefunden.");
        }
        else{
            printf("Übereinstimmung gefunden an Position %d.", result);
        }
    }

    regfree(&exp);
    return 0;
}
```

In diesem Beispiel definieren wir einen Regulären Ausdruck, der nach Zahlenblöcken sucht, und überprüfen damit den Text "12345 abcde6789". In diesem Fall wird die Ausgabe "Übereinstimmung gefunden an Position 0." sein, da der Zahlenteil des Textes genau am Anfang steht.

## Deep Dive
Reguläre Ausdrücke können mit verschiedenen Metazeichen noch komplexer gestaltet werden. Zum Beispiel kann das Zeichen "." verwendet werden, um jedes beliebige Zeichen zu matchen, oder das Zeichen "^" um das Muster nur am Anfang des Textes zu suchen. Auch die Verwendung von Gruppierungen mithilfe von runden Klammern ist möglich, um Teilausdrücke zu definieren.

Eine ausführliche Liste der verfügbaren Metazeichen und deren Funktionsweise findet man in der Dokumentation von "regex.h".

Es ist außerdem wichtig zu wissen, dass Reguläre Ausdrücke in C standardmäßig keine Unicode-Unterstützung bieten. Wenn man also mit Zeichen außerhalb des ASCII-Zeichensatzes arbeiten möchte, muss dies speziell berücksichtigt werden.

## Siehe Auch
- [Reguläre Ausdrücke in C - Dokumentation](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions-in-C.html)
- [Unicode in Regulären Ausdrücken in C](https://www.regular-expressions.info/unicode.html)