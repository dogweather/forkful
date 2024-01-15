---
title:                "Maiuscolizzazione di una stringa"
html_title:           "C: Maiuscolizzazione di una stringa"
simple_title:         "Maiuscolizzazione di una stringa"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##Perchè

Spesso capita di dover manipolare dati di testo all'interno di un programma in C, e una delle operazioni più comuni è quella di convertire la prima lettera di una stringa in maiuscolo. Capitale è un'esigenza comune in molti contesti, come ad esempio nel salvataggio di un nome utente, nel formattare un titolo o nell'ordinamento di una lista di parole. In questo articolo vedremo come implementare questa funzionalità in C.

##Come fare

Per eseguire questa operazione, è necessario manipolare i singoli caratteri della stringa. La funzione `toupper()` della libreria standard `<ctype.h>` ci sarà di aiuto, in quanto prende come argomento un singolo carattere e lo converte nel corrispondente carattere maiuscolo, se presente.

```
#include <stdio.h>
#include <ctype.h>

void capitalize(char str[])
{
    int i = 0;

    // Scansione dei caratteri fino alla fine della stringa
    while (str[i] != '\0')
    {
        // Converte il carattere in maiuscolo
        str[i] = toupper(str[i]);
        i++;
    }
}

int main()
{
    // Stringa di esempio
    char stringa[] = "ciao mondo";

    // Chiamata della funzione per capitalizzare il testo
    capitalize(string);

    // Output: CIAO MONDO
    printf("%s", stringa);

    return 0;
}
```

##Approfondimento

In generale, è possibile capitalizzare anche le lettere successive alla prima, seguendo le regole di grammatica della lingua in cui si sta lavorando. Tuttavia, per evitare complicazioni di questo tipo, è consigliabile utilizzare una libreria esterna specifica per la lingua desiderata.

##Vedi anche

- [Funzione `toupper()` della libreria `<ctype.h>`](https://man7.org/linux/man-pages/man3/toupper.3.html)
- [Libreria per la formattazione dei testi in italiano in C](https://github.com/enricobacis/libitalian)