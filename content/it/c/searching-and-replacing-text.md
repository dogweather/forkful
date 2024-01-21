---
title:                "Ricerca e sostituzione del testo"
date:                  2024-01-20T17:57:10.166317-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Cercare e sostituire testo è il processo di localizzare stringhe specifiche e cambiarle con altre. I programmatori lo fanno per aggiornare codici, correggere errori o per cambiamenti di massa senza smenarci le dita.

## How to: (Come fare:)
```C
#include <stdio.h>
#include <string.h>

void searchAndReplace(char *s, const char *oldW, const char *newW) {
    char buffer[1024];
    char *pos, *temp = s;
    int oldWLen = strlen(oldW);
    int newWLen = strlen(newW);
    
    // Il buffer inizia vuoto.
    buffer[0] = '\0';
    
    // Finché ci sono occorrenze, continua.
    while ((pos = strstr(temp, oldW)) != NULL) {
        // Copia fino all'occorrenza trovata.
        strncat(buffer, temp, pos - temp);
        
        // Aggiungi la nuova parola al buffer.
        strcat(buffer, newW);
        
        // Sposta 'temp' oltre la parola vecchia.
        temp = pos + oldWLen;
    }
    
    // Assicurati di copiare anche la parte finale della stringa.
    strcat(buffer, temp);
    
    // Copia il buffer sulla stringa originale.
    strcpy(s, buffer);
}

int main() {
    char text[] = "il vecchio testo è vecchio e noioso.";
    const char *oldText = "vecchio";
    const char *newText = "nuovo";

    searchAndReplace(text, oldText, newText);
    printf("Testo modificato: %s\n", text);

    return 0;
}
```
Output:
```
Testo modificato: il nuovo testo è nuovo e noioso.
```

## Deep Dive (Approfondimento)
La manipolazione del testo è essenziale sin dagli albori dell'informatica. Narra la leggenda che sistemi come UNIX hanno reso queste operazioni celebri con utilità come `sed` e `awk`. In alternativa, ci sono librerie come `regex.h` in C per fare operazioni simili con espressioni regolari. In termini di implementazione, la chiave è nel garantire che il buffer sia abbastanza grande per contenere la nuova stringa. E occhio alle prestazioni quando lavori con testi giganti!

## See Also (Vedi Anche)
- Manuale di `regex.h`: https://www.man7.org/linux/man-pages/man7/regex.7.html
- Tutorial `sed`: https://www.grymoire.com/Unix/Sed.html
- Approfondimenti sulle stringhe in C: https://www.tutorialspoint.com/cprogramming/c_strings.htm