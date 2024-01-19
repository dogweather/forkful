---
title:                "Mettere in maiuscolo una stringa"
html_title:           "C: Mettere in maiuscolo una stringa"
simple_title:         "Mettere in maiuscolo una stringa"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

---

## Che Cos'è & Perché?

Capitalizzare una stringa significa convertire tutte le sue lettere in maiuscole. I programmatori lo fanno per formattare l'output o per confrontare le stringhe in modo non sensibile alla distinzione tra maiuscole e minuscole.

## Come fare:

Qui mostriamo come capitalizzare una stringa in C, usando la funzione `toupper()`. Ecco un esempio di codice:

```C
#include <ctype.h>
#include <stdio.h>

void ConvertiMaiuscolo(char s[]) {
   for(int i = 0; s[i]!= '\0'; i++){
      s[i] = toupper(s[i]);
   }
}

int main() {
   char stringa[] = "ciao mondo";
   ConvertiMaiuscolo(stringa);
   
   printf("%s\n", stringa);
   return 0;
}
```
Questo codice restituirà:

```
CIAO MONDO
```

## Approfondimento

Capitalize è un concetto vecchio quanto la programmazione stessa. Oltre a `toupper()`, una funzione built-in, la libreria C standard fornisce anche la funzione `transform` con l'locale C++ per manipolare le stringhe. Tieni però a mente che queste funzioni gestiscono caratteri ASCII standard e potrebbero non funzionare come previsto con stringhe Unicode o altri caratteri non-ASCII. Nel mondo moderno, librarie come ICU forniscono funzioni di conversione più robuste che possono gestire una vasta gamma di lingue e codifiche di caratteri.

## Vedi Anche

- [Documentazione ufficiale delle funzioni della libreria `<ctype.h>`](https://en.cppreference.com/w/c/string/byte)
- [Libreria di codifica dei caratteri internazionali ICU](http://site.icu-project.org/)
- [Tutorial su stringhe C e loro manipolazioni](https://www.programiz.com/c-programming/c-strings)