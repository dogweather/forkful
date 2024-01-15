---
title:                "Suchen und Ersetzen von Text"
html_title:           "C: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Manchmal muss man in seinem Code bestimmte Textstellen suchen und durch andere ersetzen. Dies kann aus verschiedenen Gründen notwendig werden, zum Beispiel um ein Programm effizienter zu machen oder um Fehler zu beheben.

## So funktioniert es

Die C-Standardbibliothek bietet die Funktion `str_replace`, die für das Suchen und Ersetzen von Text verwendet werden kann. Diese Funktion benötigt drei Parameter: einen Zeiger auf die zu durchsuchende Zeichenkette, einen Zeiger auf die Textstelle, die ersetzt werden soll, und einen Zeiger auf die neue Textstelle. 
Ein Beispielcode könnte wie folgt aussehen:

```C
char str[100] = "Ich liebe C++";
char* ptr = str_replace(str, "C++", "C");
printf("%s", ptr); // Ausgabe: Ich liebe C
```

In diesem Beispiel wird der Text "C++" durch "C" ersetzt und die geänderte Zeichenkette wird anschließend ausgegeben.

## Detaillierte Informationen

Die Funktion `str_replace` arbeitet immer nur auf der ersten Textstelle, die übereinstimmt. Wenn also mehrere Textstellen im Code durchsucht werden sollen, muss die Funktion mehrmals aufgerufen werden.
Außerdem gibt die Funktion einen Zeiger auf die veränderte Zeichenkette zurück, weshalb dieser in einer Variablen gespeichert werden muss, wenn er weiterverwendet werden soll.

## Siehe auch

- [C-Standardbibliothek: str_replace](https://www.cplusplus.com/reference/cstring/str_replace/)
- [Weitere nützliche C-Bibliotheksfunktionen](https://dev.to/hoangbkit/10-useful-c-standard-library-functions-1nd7)