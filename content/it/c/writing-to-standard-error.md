---
title:                "Scrivere a errori standard"
html_title:           "C: Scrivere a errori standard"
simple_title:         "Scrivere a errori standard"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Scrivere su standard error è un modo per i programmatori di inviare messaggi di errore o diagnostici al terminale durante l'esecuzione di un programma. Ciò consente loro di identificare e risolvere eventuali problemi nel codice in modo più efficiente.

## Come fare:
Di seguito sono riportati alcuni esempi di codice C che mostrano come scrivere su standard error e l'output risultante. Questi esempi assumono che sia già stato incluso il header ```<stdio.h>```.

Scrivere una semplice stringa sull'output di errore:
```
fprintf(stderr, "Questo è un messaggio di errore.\n");
```
Output:
```
Questo è un messaggio di errore.
```

Sostituire una variabile o una funzione come argomento di ```fprintf```:
```
int numero = 10;
fprintf(stderr, "Il numero è %d\n", numero);
```
Output:
```
Il numero è 10.
```

## Approfondimento:
Scrivere su standard error è stato introdotto nella versione di C del 1978 e continua ad essere utilizzato per la gestione degli errori nei programmi. Un'altra opzione per l'output di errore è la funzione ```perror```, che stampa un messaggio di errore predefinito seguito da una descrizione della causa dell'errore.

Una possibile implementazione di come scrivere su standard error potrebbe essere simile a quanto segue:
```
#define stderr (&_iob[2])
```
Qui, ```_iob``` è un array di file descriptor usato per le operazioni di input/output standard. Siccome ```stderr``` corrisponde all'elemento con indice 2 di tale array, ogni chiamata a ```fprintf``` su ```stderr``` effetua un'operazione di output su quello specifico file descriptor. Ciò è utile per distinguere l'output di errore dall'output standard.

## Vedi anche:
- [Documentazione di printf](https://www.cplusplus.com/reference/cstdio/fprintf/?kw=fprintf)
- [Tutorial su stderr e la gestione degli errori](https://www.thegeekstuff.com/2013/01/c-stderr-printf/)
- [Spiegazione dettagliata sulla differenza tra stdout e stderr](https://stackoverflow.com/questions/23643694/difference-between-stdout-and-stderr-in-plain-english)