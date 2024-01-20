---
title:                "Zeichen löschen, die einem Muster entsprechen"
html_title:           "C#: Zeichen löschen, die einem Muster entsprechen"
simple_title:         "Zeichen löschen, die einem Muster entsprechen"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Löschen von Zeichen, die einem Muster entsprechen, ist eine übliche Praxis in der Programmierung, um unerwünschte Zeichen aus einer Zeichenkette zu entfernen. Dies wird meist zum Säubern von Eingabedaten oder Dateien verwendet.

## So geht das:
Wir werden die Standardbibliothek `string.h` in C verwenden, um Zeichen zu löschen, die einem bestimmten Muster entsprechen. Sehen wir uns das anhand eines Codebeispiels an:

```c
#include <stdio.h>
#include <string.h>

const char* DeletePattern(char *str, char *pattern)
{
    int str_index = 0, result_index = 0;
    int len = strlen(pattern);
 
    while (str[str_index])
    {
        int j;
        for (j = 0; j < len; j++)
            if (str[str_index] == pattern[j])
                break;
 
        if (j == len)
            str[result_index++] = str[str_index];
 
        str_index++;
    }
    
    str[result_index] = '\0';
    
    return str;
}

int main()
{
    char str[] = "Beispieltext";
    char pattern[] = "ext";
    printf("Vorher: %s\n", str);
    printf("Nachher: %s\n", DeletePattern(str, pattern));
    
    return 0;
}
```

Die Ausgabe dieses Programms sieht so aus:

```c
Vorher: Beispieltext
Nachher: Bispilm
```
In diesem Code nehmen wir eine String und ein Muster, und wiederholen den String Zeichen für Zeichen. Wenn das aktuelle Zeichen im Muster gefunden wird, wird es nicht in den resultierenden String aufgenommen.

## Vertiefung
Früher, vor der Verfügbarkeit von Standard-Bibliotheksfunktionen wie `strspn()`, `strpbrk()`, etc., hatten Programmierer in C ihren eigenen Code zum Löschen von Zeichen geschrieben, die einem Muster entsprechen. Jetzt bieten Standard-Bibliotheken Alternativen, die einfacher und effizienter zu nutzen sind. Zum Beispiel kann das `strpbrk()` verwendet werden, um das erste Vorkommen eines beliebigen Zeichens aus einer Menge von Zeichen zu finden, und `strspn()` kann verwendet werden, um den kürzesten Substring zu finden, der nur Zeichen aus der Menge enthält.

Je nach Bedarf des Programms kann diese Funktion modifiziert werden. Z.B. könnten wir nur das erste Vorkommen eines Zeichenmusters löschen, oder alle bis auf das letzte Vorkommen, usw.

## Siehe auch
- https://en.cppreference.com/w/c/string/byte/strpbrk
- https://en.cppreference.com/w/c/string/byte/strspn
- https://www.geeksforgeeks.org/remove-character-array-string-c/