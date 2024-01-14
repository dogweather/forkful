---
title:    "C: Die Länge eines Strings finden"
keywords: ["C"]
---

{{< edit_this_page >}}

## Warum
Der Grund, warum man die Länge einer Zeichenkette (String) in C finden möchte, ist oft, um die Größe des Speicherbereichs zu bestimmen, den die Zeichenkette benötigt. Dies ist besonders wichtig bei der Arbeit mit dynamischen Zeichenketten, die während der Programmausführung wachsen können.

## Wie geht's
Es gibt verschiedene Möglichkeiten, die Länge einer Zeichenkette in C zu finden. Die einfachste Methode besteht darin, die Standardbibliotheksfunktion `strlen()` zu verwenden. Diese Funktion gibt die Anzahl der Zeichen in der Zeichenkette zurück, ohne das Null-Terminierungszeichen mitzuzählen.

```C
#include <stdio.h>
#include <string.h>

int main(){
   char str[50] ="Hallo Welt";
   int length = strlen(str);

   printf("Länge des Strings: %d", length);
   
   return 0;
}
```
Output:
```C
Länge des Strings: 10
```

Eine andere Möglichkeit ist, eine Schleife zu verwenden, um jeden Buchstaben in der Zeichenkette zu zählen, bis das Null-Terminierungszeichen erreicht ist.

```C
#include <stdio.h>

int main(){
   char str[50] ="Hallo Welt";
   int length = 0;

   while (str[length] != '\0')
       length++;

   printf("Länge des Strings: %d", length);
   
   return 0;
}
```
Output:
```C
Länge des Strings: 10
```

## Tiefergehende Informationen
Es ist wichtig zu beachten, dass es bei der Länge einer Zeichenkette um die Anzahl der Zeichen geht, nicht um die Anzahl der Bytes, die sie belegt. Eine Zeichenkette kann unterschiedliche Anzahlen von Bytes belegen, abhängig von der verwendeten Zeichencodierung. Zum Beispiel benötigt ein einzelnes Zeichen in UTF-8 möglicherweise mehr als ein Byte, während es in ASCII nur ein Byte benötigt.

Wenn Sie eine genauere Kontrolle über die Speichergröße Ihrer Zeichenkette benötigen, müssen Sie möglicherweise die Anzahl der Bytes der Zeichenkette über die Funktion `sizeof()` bestimmen. Diese Funktion gibt die Gesamtgröße eines Datentyps (einschließlich Null-Terminierungszeichen) zurück.

## Siehe auch
- [C-Strings](https://www.programiz.com/c-programming/c-strings)
- [The strlen() function in C](https://www.geeksforgeeks.org/strlen-function-in-c/)
- [The sizeof() operator in C](https://www.tutorialspoint.com/cprogramming/c_sizeof_operator.htm)