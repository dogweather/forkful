---
title:                "C: Verkettung von Zeichenfolgen"
simple_title:         "Verkettung von Zeichenfolgen"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/concatenating-strings.md"
---

{{< edit_this_page >}}

# Warum

Strings sind eine der grundlegenden Datentypen in der Programmierung und werden verwendet, um Textinformationen zu speichern und zu manipulieren. Eine der nützlichsten Funktionen, die wir in der Arbeit mit Strings verwenden können, ist die Möglichkeit, sie zu verketten oder in einfacheren Worten zu verbinden. Indem wir mehrere Strings miteinander verbinden, können wir komplexe Texte erstellen und unsere Programme flexibler und vielseitiger machen. In diesem Blogbeitrag werden wir uns genauer ansehen, wie man Strings in C programmatisch verketten kann.

# Wie es geht

Um Strings in C zu verketten, gibt es verschiedene Ansätze. Der einfachste Weg ist die Verwendung der Funktion `strcat()`, die in der Bibliothek `string.h` definiert ist. Diese Funktion nimmt zwei Argumente entgegen: den Zielstring und den Quellstring. Sie verbindet den Quellstring mit dem Zielstring und gibt den zusammengesetzten String als Ergebnis zurück.

```C
#include <stdio.h>
#include <string.h>

int main(){
    char str1[20] = "Hallo ";
    char str2[10] = "Welt";
    
    strcat(str1, str2);
    
    printf("%s\n", str1);
    
    return 0;
}

// Output: Hallo Welt
```

Wir können auch die Funktion `sprintf()` verwenden, um Strings zu verketten. Diese Funktion funktioniert ähnlich wie `printf()`, jedoch wird der formatierte String nicht auf dem Bildschirm ausgegeben, sondern in einer Variablen gespeichert.

```C
#include <stdio.h>

int main(){
    char str1[20] = "Hallo ";
    char str2[10] = "Welt";
    char result[30];
    
    sprintf(result, "%s%s", str1, str2);
    
    printf("%s\n", result);
    
    return 0;
}

// Output: Hallo Welt
```

Ein weiterer Ansatz ist die Verwendung der Operator `+` und der Funktion `strlen()` aus der `string.h` Bibliothek.

```C
#include <stdio.h>
#include <string.h>

int main(){
    char str1[20] = "Hallo ";
    char str2[10] = "Welt";
    int i, j;
    
    // Die Länge von str1 berechnen
    i = strlen(str1);
    
    // str2 an das Ende von str1 anhängen
    for(j = 0; str2[j] != '\0'; i++, j++){
        str1[i] = str2[j];
    }
    
    // Den Nullterminator hinzufügen
    str1[i] = '\0';
    
    printf("%s\n", str1);
    
    return 0;
}

// Output: Hallo Welt
```

# Tiefer eintauchen

Es gibt noch viele andere Wege, um Strings in C zu verketten. Wir können beispielsweise Funktionen wie `strncat()` oder `memcpy()` verwenden, um Strings zusammenzufügen. Auch das Arbeiten mit Pointern kann nützlich sein, um effizientere Verkettungen zu erreichen. Es ist wichtig zu verstehen, dass die Länge des Zielstrings ausreichend groß sein muss, um den Quellstring aufzunehmen, da es sonst zu Speicherproblemen kommen kann. Durch die Verwendung der oben genannten Methoden können wir verhindern, dass unser Programm abstürzt oder unerwartete Ergebnisse liefert.

# Siehe auch

- [Offizielle Dokumentation für String-Funktionen in C](https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm)
- [Tutorial zur Verkettung von Strings in C](https://www.programiz.com/c-programming/c-strings-concatenation)
- [Arbeitsweise von Pointern in C](https://www.geeksforgeeks.org/pointers-in-c-and-c-set-1-introduction-arithmetic-and-array/)