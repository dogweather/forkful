---
title:                "C: Verkettung von Zeichenfolgen"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Das Verknüpfen von Strings ist eine häufige Aufgabe bei der Programmierung in C. Es ermöglicht es uns, gezielt Texte oder Zeichenfolgen zu manipulieren und zu verändern. Ob es darum geht, Benutzereingaben zu überprüfen oder Text für die Ausgabe formatiert auszugeben, das Zusammenfügen von Strings ist eine grundlegende Technik, die in vielen C Programmen verwendet wird.

## Wie

Die grundlegende Syntax für das Verknüpfen von Strings in C ist durch die Funktion `strcat()` gegeben, die innerhalb der Bibliothek `<string.h>` definiert ist. Diese Funktion nimmt zwei Parameter - den ersten Parameter ist der Zielspeicherort, an dem die Strings verknüpft werden sollen, und der zweite Parameter ist der String, der hinzugefügt werden soll. In der Praxis sieht die Verwendung der `strcat()` Funktion wie folgt aus:

```C
#include <stdio.h>
#include <string.h>

int main(){
    char str1[20] = "Hallo ";
    char str2[]= "Welt";
    
    strcat(str1, str2);
    printf("%s", str1);
    
    return 0;
}
```

Die Ausgabe dieses Codeschnipsels wäre `Hallo Welt`.

Es ist wichtig zu beachten, dass der Zielspeicherort genügend Platz haben muss, um den verknüpften String aufzunehmen. Andernfalls kann es zu Speicherüberschreitungen führen, was zu unerwartetem Verhalten oder Abstürzen des Programms führen kann.

## Deep Dive

Eine interessante Tatsache über die `strcat()` Funktion ist, dass sie immer den ersten Parameter zurückgibt, und nicht den verknüpften String. Das bedeutet, dass es möglich ist, die `strcat()` Funktion innerhalb einer anderen Funktion oder zuweisungsanweisung zu verwenden. Zum Beispiel:

```C
#include <stdio.h>
#include <string.h>

int main(){
    char str1[20] = "Hallo ";
    char str2[]= "Welt";
    
    char *concat = strcat(str1, str2);
    printf("%s", concat);
    
    return 0;
}
```

In diesem Fall wird die `strcat()` Funktion verwendet, um den verknüpften String in der Variablen `concat` zu speichern, anstatt ihn direkt auszugeben.

Es gibt auch eine ähnliche Funktion `strncat()`, die einen zusätzlichen Parameter hat, um die maximale Anzahl von Zeichen festzulegen, die an den verknüpften String angehängt werden sollen. Dies kann hilfreich sein, um Speicherüberschreitungen zu vermeiden, wenn der Zielspeicherort begrenzt ist.

## Siehe auch

- [Dokumentation der `strcat()` Funktion in C (Englisch)](https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm)
- [Weitere Informationen über die `<string.h>` Bibliothek (Deutsch)](https://openbook.rheinwerk-verlag.de/c_von_a_bis_z/002_c_grundlagen_001.htm)