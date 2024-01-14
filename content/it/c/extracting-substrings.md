---
title:                "C: Estrazione di sottostringhe"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Molti programmatori si trovano spesso ad affrontare il problema di estrarre una sottostringa da una stringa più grande in C. Magari si vuole ottenere solamente una parte di una stringa più lunga o si vuole manipolare una stringa per ottenere un output specifico. In questo caso, l'estrazione di sottostringhe può essere una tecnica molto utile e ci permette di gestire le stringhe in modo più flessibile.

## Come fare

Per estrarre una sottostringa in C, possiamo utilizzare la funzione `strncpy()` che copia un numero specificato di caratteri da una stringa di origine ad una stringa di destinazione. La sintassi per utilizzare questa funzione è la seguente:

```C
strncpy(dest, source, n)
```
dove `dest` è la stringa di destinazione in cui verrà copiata la sottostringa, `source` è la stringa di origine da cui verranno estratti i caratteri e `n` è il numero di caratteri da copiare.

Ad esempio, se vogliamo estrarre i primi 5 caratteri di una stringa `str`, possiamo utilizzare la funzione `strncpy()` in questo modo:
```C
char str[10] = "Ciao mondo";
char sub[6];
strncpy(sub, str, 5);
printf("La sottostringa è %s", sub);
```
Questo produrrà l'output `La sottostringa è Ciao`.

È importante notare che la sottostringa estratta non avrà un carattere terminatore `\0` alla fine, quindi dobbiamo assicurarci di aggiungerlo manualmente alla fine della sottostringa se vogliamo utilizzarla come una stringa completa.

## Approfondimento

La funzione `strncpy()` non solo ci permette di estrarre una sottostringa da una stringa di origine, ma ci consente anche di specificare la lunghezza della sottostringa desiderata. In questo modo, possiamo estrarre sia una sottostringa di lunghezza fissa che una sottostringa di lunghezza variabile.

Inoltre, C offre anche altre funzioni per l'estrazione di sottostringhe, come ad esempio `strncat()` e `substring()`. È importante leggere la documentazione di queste funzioni per capire come utilizzarle correttamente e quali potrebbero essere i loro limiti.

## Vedi anche

1. [Documentazione ufficiale di C](https://devdocs.io/c/)
2. [Come usare le funzioni di stringa in C](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
3. [Esempi di codice per lavorare con stringhe in C](https://www.includehelp.com/c/string-functions-in-c-learnc.aspx)