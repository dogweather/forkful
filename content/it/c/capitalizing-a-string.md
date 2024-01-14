---
title:    "C: Maiuscolizzare una stringa"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitare una stringa è un'operazione comune che viene eseguita in molti programmi. Ciò può essere utile per uniformare il formato dei dati o per presentare in modo più professionale i risultati di un'applicazione. In questo articolo, scopriremo come utilizzare il linguaggio di programmazione C per eseguire questa semplice operazione.

## Come fare

Per capitalizzare una stringa in C, dobbiamo utilizzare la funzione `toupper()` inclusa nella libreria `ctype.h`. Questa funzione prende come input un carattere e lo converte nella sua forma maiuscola corrispondente. Possiamo utilizzare un ciclo `for` per iterare attraverso la stringa e applicare la funzione `toupper()` a ogni carattere. Di seguito è riportato un esempio di codice:

```C
#include <stdio.h>
#include <ctype.h>

int main() {
    // Definiamo una stringa di esempio
    char string[] = "ciao a tutti";
    int i;

    // Iteriamo attraverso la stringa
    for (i = 0; string[i] != '\0'; i++) {
        // Capitalizziamo ogni carattere
        string[i] = toupper(string[i]);
    }

    // Stampiamo la stringa risultante
    printf("La stringa in maiuscolo è: %s", string);

    return 0;
}
```
L'output di questo codice sarà "CIAO A TUTTI".

È importante notare che la funzione `toupper()` converte solo i caratteri dalla 'a' alla 'z' in maiuscolo. Se la stringa contiene caratteri speciali o numeri, questi rimarranno invariati. È anche possibile utilizzare la funzione `tolower()` per convertire una stringa in minuscolo.

## Approfondimento

Ci sono alcune cose da tenere a mente quando si utilizza la funzione `toupper()` per capitalizzare una stringa in C. In primo luogo, è necessario includere la libreria `ctype.h` nel nostro programma, in quanto essa contiene le funzioni di conversione dei caratteri. Inoltre, la funzione `toupper()` accetta solo caratteri firmati, quindi se si lavora con caratteri non firmati, è necessario utilizzare prima la funzione `cast` per convertirli.

Un altro aspetto importante da considerare è la localizzazione della stringa. La funzione `toupper()` può comportarsi in modo diverso a seconda del sistema operativo e delle impostazioni di localizzazione. Per garantire una conversione uniforme, è possibile utilizzare la funzione `setlocale()` per impostare la localizzazione desiderata.

## Vedi anche

- [Funzione `toupper()` su cplusplus.com](https://www.cplusplus.com/reference/cctype/toupper/)
- [Funzione `setlocale()` su cplusplus.com](https://www.cplusplus.com/reference/clocale/setlocale/)