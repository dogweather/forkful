---
title:    "C: Suchen und Ersetzen von Text"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie jemals mit großen Textdateien oder umfangreichen Codeprojekten gearbeitet haben, wissen Sie, wie mühsam es sein kann, Text manuell zu suchen und zu ersetzen. Aber zum Glück gibt es in der C-Programmierung eine einfache Möglichkeit, dies automatisch zu erledigen. Durch das Schreiben von Code, der Text sucht und ersetzt, können Sie Zeit sparen und Fehler minimieren.

## Wie geht das

Das Suchen und Ersetzen von Text in C ist einfach mit der Verwendung der Funktion `strstr()` zu erreichen. Diese Funktion sucht nach dem ersten Vorkommen eines bestimmten Teilstrings in einem größeren String und gibt einen Zeiger auf die erste Position des Teilstrings zurück. Die Syntax sieht wie folgt aus:

```C
char *strstr(const char *haystack, const char *needle);
```
Um diese Funktion zu verwenden, müssen Sie zunächst den zu durchsuchenden Text und den zu suchenden Teilstring angeben. Der folgende Code zeigt, wie man `strstr()` in einem einfachen Programm verwenden kann:

```C
#include <stdio.h>
#include <string.h>

int main()
{
    // Text, in dem gesucht wird
    char text[] = "Informatik ist toll";

    // Teilstring, der ersetzt werden soll
    char needle[] = "toll";

    // Text durchsuchen und Zeiger auf das erste Vorkommen des Teilstrings erhalten
    char *position = strstr(text, needle);

    // Teilstring durch neuen Text ersetzen
    sprintf(position, "fantastisch");

    // Ergebnis ausgeben
    printf("%s", text);

    return 0;
}
```
Dieses Programm durchsucht den Text "Informatik ist toll" nach dem Teilstring "toll" und ersetzt ihn dann durch "fantastisch". Das Ergebnis lautet "Informatik ist fantastisch".

Natürlich ist dies nur ein einfaches Beispiel und es gibt viele weitere Möglichkeiten, die `strstr()` Funktion in Ihrem Code zu verwenden. Es kann auch hilfreich sein, die Funktion `strchr()` zu kennen, die nach dem ersten Vorkommen eines bestimmten Zeichens in einem String sucht.

## Tiefere Einblicke

Für anspruchsvollere Such- und Ersatzoperationen kann es sinnvoll sein, in die Verwendung von regulären Ausdrücken einzutauchen. Reguläre Ausdrücke sind ein leistungsstarkes Werkzeug, das in vielen Programmiersprachen verwendet wird, um nach bestimmten Mustern in Texten zu suchen und sie zu ersetzen. Die C-Standardbibliothek bietet jedoch keine native Unterstützung für reguläre Ausdrücke, daher müssen Sie möglicherweise eine externe Bibliothek wie PCRE (Perl-kompatible reguläre Ausdrücke) nutzen.

Eine weitere Sache, die Ihren Code effektiver machen kann, ist die Verwendung von Befehlszeilenargumenten. Durch das Lesen von Benutzereingaben aus der Befehlszeile können Sie Ihr Such- und Ersatzprogramm flexibler gestalten und es dem Benutzer ermöglichen, verschiedene Optionen anzugeben, wie z.B. welchen Teilstring er sucht oder welcher Text ersetzt werden soll.

## Siehe auch

- [Die offizielle C-Dokumentation zur strstr() Funktion](https://en.cppreference.com/w/c/string/byte/strstr)
- [Eine einfache Einführung in reguläre Ausdrücke in C](https://www.linuxjournal.com/article/1130)
- [PCRE-Bibliothek](https://www.pcre.org/) für die Verwendung von regulären Ausdrücken in C