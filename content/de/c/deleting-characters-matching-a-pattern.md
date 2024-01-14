---
title:                "C: Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist eine nützliche Fähigkeit in der C-Programmierung. Es kann bei der Bearbeitung von Strings oder beim Filtern von Benutzereingaben hilfreich sein.

# Wie geht es

Um Zeichen anhand eines Musters zu löschen, können wir die Funktion "strpbrk" verwenden. Diese Funktion durchsucht einen String nach dem Muster und gibt einen Zeiger auf das erste Vorkommen zurück. Dann können wir den Teil des Strings, der vor dem gefundenen Zeichen liegt, mit dem Teil nach dem gefundenen Zeichen zusammenfügen, um das gewünschte Ergebnis zu erhalten.

```C
#include <stdio.h>
#include <string.h>

int main(void)
{
  char str[100] = "Die Sonne scheint, der Himmel ist blau.";

  printf("Originaler String: %s\n", str);
  char* ptr;

  // Suche nach dem Muster ","
  ptr = strpbrk(str, ",");

  // Füge den Teil des Strings vor dem gefundenen Zeichen mit dem Teil danach zusammen
  strcpy(ptr, ptr + 1);

  printf("Geänderter String: %s\n", str);

  return 0;
}
```

# Tiefer Einblick

Die Funktion "strpbrk" durchläuft den String von links nach rechts und vergleicht jedes Zeichen mit denen im Muster angegebenen. Wenn ein Treffer gefunden wird, wird ein Zeiger auf das entsprechende Zeichen im String zurückgegeben. Wenn das Ende des Musters erreicht ist, wird NULL zurückgegeben, was angibt, dass kein Treffer gefunden wurde.

Es ist auch möglich, die Funktion "strtok" zu verwenden, um einen String anhand eines Trennzeichen in einzelne Tokens zu zerlegen. Dann können wir die gewünschten Tokens zusammenfügen, um das Muster zu löschen.

# Siehe auch

1. [strpbrk-Funktion](https://www.tutorialspoint.com/c_standard_library/c_function_strpbrk.htm)
2. [strtok-Funktion](https://www.tutorialspoint.com/c_standard_library/c_function_strtok.htm)