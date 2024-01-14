---
title:    "C: Umwandeln eines Strings in Kleinbuchstaben"
keywords: ["C"]
---

{{< edit_this_page >}}

##Warum

Das Konvertieren von Strings in Kleinbuchstaben ist eine häufige Aufgabe bei der Verarbeitung von Benutzereingaben in Programmen. Es kann helfen, die Eingabe einheitlicher und besser lesbar zu machen.

##Wie geht man vor?

Um einen String in Kleinbuchstaben umzuwandeln, gibt es mehrere Möglichkeiten in der Programmiersprache C. Eine beliebte Methode ist die Verwendung der Standardbibliotheksfunktion `tolower()`, die jeden Buchstaben in einem String in Kleinbuchstaben umwandelt. Hier ist ein einfaches Beispiel:

```C
#include <stdio.h>
#include <ctype.h>

int main(){
    
    char string[] = "Hallo WELT!";
    
    for(int i = 0; string[i]; i++){
        string[i] = tolower(string[i]);
    }

    printf("Der umgewandelte String lautet: %s\n", string);

    return 0;
}
```
Output: `Der umgewandelte String lautet: hallo welt!`

Ein weiterer Ansatz ist die Verwendung der ASCII-Werte der Buchstaben. Da Großbuchstaben und Kleinbuchstaben im ASCII-Code aufeinanderfolgende Werte haben, können wir einfach die Differenz von 32 zu dem ASCII-Wert eines Großbuchstaben addieren, um ihn in den entsprechenden Kleinbuchstaben umzuwandeln. Dieser Ansatz kann effizienter sein als die Verwendung von `tolower()`. Hier ist ein Beispiel:

```C
#include <stdio.h>

int main(){
    
    char string[] = "Hallo WELT!";
    
    for(int i = 0; string[i]; i++){
        if(string[i] >= 'A' && string[i] <= 'Z') { //überprüft, ob das Zeichen ein Großbuchstabe ist
            string[i] += 32; //fügt 32 zu dem ASCII-Wert hinzu, um ihn in den entsprechenden Kleinbuchstaben umzuwandeln
        }
    }

    printf("Der umgewandelte String lautet: %s\n", string);

    return 0;
}
```
Output: `Der umgewandelte String lautet: hallo welt!`

Es gibt noch weitere Möglichkeiten, einen String in Kleinbuchstaben umzuwandeln, aber diese beiden sind die am häufigsten verwendeten Methoden.

##Tiefer Einblick

Bei der Verwendung von `tolower()` ist es wichtig zu beachten, dass die Funktion nur für Buchstaben im ASCII-Bereich funktioniert. Für andere Zeichen, wie zum Beispiel Umlaute in der deutschen Sprache, muss man alternative Ansätze finden.

Außerdem gibt es in der Programmierung oft die Notwendigkeit, Strings zu vergleichen. Wenn man jedoch einen String in Kleinbuchstaben umwandelt, kann dies die Ergebnisse von Vergleichen beeinflussen. Wenn es wichtig ist, die Groß- und Kleinbuchstaben zu berücksichtigen, muss man sich möglicherweise alternative Lösungen überlegen. 

##Siehe auch

- [ASCII-Tabelle](https://www.asciitable.com/)
- [ASCII und Zeichensätze](https://www.theasciicode.com.ar/)
- [Strings in C](https://www.programiz.com/c-programming/c-strings)