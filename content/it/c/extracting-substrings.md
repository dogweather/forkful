---
title:                "Estrazione di sottostringhe"
html_title:           "C: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Se stai cercando un modo per manipolare e lavorare con stringhe in modo più efficiente, allora imparare come estrarre sottostringhe può essere utile per te. L'estrazione di sottostringhe è un'operazione comune e utile nella programmazione, specialmente quando si lavora con input di grandi dimensioni o con stringhe che contengono informazioni specifiche.

## Come fare

Per estrarre una sottostringa da una stringa in C, puoi utilizzare la funzione `strncpy()`. Questa funzione prende come argomenti tre parametri: la stringa di destinazione, la stringa di origine e il numero di caratteri da copiare. Ad esempio:

```C
char source[] = "Questo è un esempio di stringa";
char dest[20];

strncpy(dest, source, 10);
dest[10] = '\0'; // Aggiungiamo il terminatore di stringa
printf("La sottostringa estratta è %s", dest);
```

L'output di questo codice sarà `Questo è u`. Come puoi vedere, la sottostringa è stata estratta dalle prime 10 lettere della stringa originale. Assicurati di aggiungere il terminatore di stringa quando utilizzi la funzione `strncpy()`, in modo da evitare problemi di memoria.

Se vuoi estrarre una sottostringa da una posizione specifica in una stringa, puoi utilizzare la funzione `strncpy()` in combinazione con la funzione `strlen()`. Ad esempio, se vogliamo estrarre le ultime 5 lettere dalla stringa originale, possiamo utilizzare il seguente codice:

```C
char source[] = "Questa è la mia stringa";
char dest[6];

int index = strlen(source) - 5; // Otteniamo l'indice del primo carattere da copiare
strncpy(dest, source + index, 5);
dest[5] = '\0'; // Aggiungiamo il terminatore di stringa
printf("La sottostringa estratta è %s", dest);
```

L'output di questo codice sarà `string`. Utilizzando la funzione `strlen()`, abbiamo ottenuto l'indice del primo carattere che volevamo copiare e poi abbiamo utilizzato la funzione `strncpy()` per copiare i successivi 5 caratteri.

## Approfondimento

Oltre alla funzione `strncpy()`, esistono anche altre funzioni utili per estrarre sottostringhe in C, come ad esempio `strchr()` e `strstr()`. Queste funzioni permettono di trovare la posizione di un carattere o di una sottostringa all'interno di una stringa e di utilizzarlo come indice per la funzione `strncpy()`.

Inoltre, è importante ricordare che la libreria `string.h` contiene molte funzioni ed è sempre una buona idea consultarla per scoprire ulteriori funzionalità utili.

## Vedi anche

- [Funzione strncpy() su GeeksforGeeks] (https://www.geeksforgeeks.org/strncpy-cpp-reference/)
- [Libreria string.h su C Reference] (https://www.cprogramming.com/reference/string.h/)