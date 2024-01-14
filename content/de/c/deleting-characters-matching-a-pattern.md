---
title:    "C: Löschen von Zeichen, die einem Muster entsprechen"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Manchmal muss man spezifische Zeichen aus einer Zeichenfolge löschen, sei es aus Gründen der Formatierung oder zur Datenbereinigung. In der Programmierung kann man dieses Problem mit dem Löschen von Zeichen entsprechend einem Muster lösen.

## Wie

Die gängige Methode zum Löschen von Zeichen basiert auf der Verwendung von Schleifen und bedingter Anweisungen. Wir durchlaufen die Zeichenfolge und überprüfen jedes Zeichen auf Übereinstimmung mit dem Muster. Wenn das Zeichen dem Muster entspricht, wird es aus der Zeichenfolge entfernt. Hier ist ein Beispiel in C-Code:

```C
#include <stdio.h>

void deleteChar(char *str, char pattern){
    int i, j;
    for(i = 0; str[i] != '\0'; i++){
        if(str[i] == pattern){
            for(j = i; str[j] != '\0'; j++){
                str[j] = str[j+1];
            }
            i--;
        }
    }
}

int main(){
    char string[] = "Hallo Welt!";
    deleteChar(string, 'l');
    printf("%s", string);
    return 0;
}
```

Das obige Beispiel zeigt eine Funktion, die einzelne Zeichen aus einer Zeichenfolge entfernt, basierend auf einem übergebenen Muster. In diesem Fall wird jedes 'l' aus der Zeichenfolge "Hallo Welt!" entfernt. Die Funktion durchläuft die Zeichenfolge und entfernt jedes gefundene Zeichen, indem sie die folgenden Zeichen einen Index nach vorne verschiebt. Das Ergebnis wird dann auf der Konsole ausgegeben und "Hao Wett!" angezeigt.

## Deep Dive

Es gibt jedoch noch weitere Möglichkeiten, Zeichen entsprechend einem Muster zu löschen, wie zum Beispiel die Verwendung von Zeichenkettenmanipulationsfunktionen wie `strtok()` oder `strchr()`. Diese Funktionen können auch verwendet werden, um mehrere Zeichen in einer Zeichenfolge zu entfernen. Es ist wichtig, auf die genaue Verwendung der jeweiligen Funktion zu achten, um unerwünschte Ergebnisse zu vermeiden.

## Siehe auch

- [C-Referenz: String-Operationen](https://www.programiz.com/c-programming/string-handling-functions)
- [Tutorial: Einführung in die Zeichenkettenmanipulation in C](https://www.guru99.com/c-strings.html)
- [C-Programmierbeispiele für die Zeichenkettenmanipulation](https://www.geeksforgeeks.org/c-programming-language)