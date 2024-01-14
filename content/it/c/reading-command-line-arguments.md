---
title:    "C: Lettura degli argomenti della riga di comando"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##Perché

Se sei un programmatore C, probabilmente hai sentito parlare dei "command line arguments" (argomenti della riga di comando). Questo concetto è molto importante perché ti consente di fornire input al tuo programma direttamente dalla riga di comando. Leggere gli argomenti della riga di comando può sembrare intimidatorio, ma in realtà è molto semplice e ti permetterà di migliorare le tue abilità di programmazione.

##Come fare

Per leggere gli argomenti della riga di comando in C, è necessario utilizzare la funzione "main()" che è il punto di ingresso di tutti i programmi C. Di seguito è riportato un esempio di codice che ti mostrerà come leggere un argomento dalla riga di comando e stamparlo sulla console:

```C
#include <stdio.h>

int main(int argc, char *argv[])
{
  //argc è il numero di argomenti passati dalla riga di comando
  //argv è un array di stringhe contenente gli argomenti
  //il primo argomento (argv[0]) è sempre il nome del programma
  //quindi il nostro argomento sarà argv[1]
  printf("Il tuo argomento è: %s", argv[1]);

  return 0;
}
```

Se eseguiamo questo programma dalla riga di comando e gli passiamo un argomento, otterremo l'output seguente:

```
$ ./programma argomento
Il tuo argomento è: argomento
```

Come puoi vedere, è abbastanza semplice! Il trucco è capire come utilizzare la funzione "main()" e come accedere ai vari argomenti tramite l'array "argv".

##Approfondimento

Ora che hai una comprensione di base di come leggere i command line arguments in C, puoi approfondire ulteriormente il concetto. Potresti voler gestire più di un argomento, o convertire gli argomenti in tipi di dati diversi, come ad esempio numeri interi o float.

Inoltre, ci sono alcune librerie di utilità disponibili per semplificare la lettura degli argomenti della riga di comando, come ad esempio "getopt".

##Vedi anche

- [Tutorial su come leggere gli argomenti della riga di comando in C](https://www.learn-c.org/en/Command_line_arguments)
- [Documentazione ufficiale di "main()" in C](https://en.cppreference.com/w/c/language/main_function)
- [Articolo su "getopt" in C](https://www.gnu.org/software/libc/manual/html_node/Example-of-Getopt.html#Example-of-Getopt)