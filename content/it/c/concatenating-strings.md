---
title:                "C: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

La concatenazione di stringhe è una delle operazioni più comuni quando si scrive codice in C. Essa permette di unire due o più stringhe, creando una nuova stringa più lunga. Questo può essere utile in molte situazioni, come ad esempio quando si vogliono unire delle stringhe di input inserite dall'utente o quando si vuole creare una stringa di output per il programma.

## Come Fare

Per concatenare due stringhe, è necessario seguire alcuni semplici passaggi:

1. Definire le stringhe da concatenare
```C 
char str1[] = "Ciao ";
char str2[] = "mondo!";
```

2. Creare una nuova stringa in cui verranno unite le due stringhe
```C
char result[14];
```

3. Utilizzare la funzione `strcat` per concatenare le stringhe
```C
strcpy(result, str1);  // copia la prima stringa nel risultato
strcat(result, str2);  // aggiunge la seconda stringa al risultato
```

4. Stampare il risultato
```C
printf("%s \n", result); // output: Ciao mondo!
```

Come si può vedere nell'esempio, la funzione `strcat` unisce la seconda stringa alla fine della prima, quindi è importante che la nuova stringa abbia abbastanza spazio per contenere entrambe le stringhe.

## Approfondimento

Nel linguaggio C, le stringhe sono rappresentate da un array di caratteri terminato dal carattere nullo (`\0`). La funzione `strcat` legge la prima stringa fino alla fine (carattere nullo) e poi aggiunge la seconda stringa a partire dal primo carattere. Ciò significa che l'array di destinazione deve essere abbastanza grande da contenere sia la prima che la seconda stringa insieme al carattere nullo finale.

Inoltre, è importante tenere presente che entrambe le stringhe devono avere lo stesso tipo di dati, altrimenti si otterranno errori di compilazione.

## Vedi Anche

- [Tutorial su stringhe in C](https://www.programiz.com/c-programming/c-strings)
- [Documentazione ufficiale di C su funzioni per le stringhe](https://www.tutorialspoint.com/c_standard_library/string_h.htm)