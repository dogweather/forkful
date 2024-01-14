---
title:    "C: Lettura di un file di testo"
keywords: ["C"]
---

{{< edit_this_page >}}

## Perché
Quando si programma in linguaggio C, spesso ci si trova a dover leggere un file di testo contenente informazioni importanti per il nostro programma. In questo blog post, illustreremo come effettuare questa operazione in modo semplice ed efficiente.

## Come Fare
Per prima cosa, dobbiamo dichiarare una variabile di tipo `FILE` che conterrà il nostro file di testo. Utilizziamo la funzione `fopen` per aprire il file in modalità lettura "r". Ad esempio:

```C
FILE *fp;
fp = fopen("mio_file.txt", "r");
```

A questo punto, possiamo leggere il contenuto del file utilizzando la funzione `fgets`, che legge una riga di testo alla volta. Utilizziamo un ciclo `while` per leggere tutte le righe fino a quando non arriviamo alla fine del file. Esempio di codice completo:

```C
#include <stdio.h>
#include <stdlib.h>
 
int main()
{
   FILE *fp;
   char riga[255];
 
   fp = fopen("mio_file.txt", "r"); //apriamo il file in modalità lettura "r"
 
   while (fgets(riga, 255, fp) != NULL) //leggi una riga fino a quando non arrivi alla fine del file
   {
      printf("%s", riga); //stampa la riga
   }
 
   fclose(fp); //chiudiamo il file
 
   return 0;
}
```

Se il tuo file di testo contiene numeri, puoi utilizzare la funzione `fscanf` per leggere e assegnare i valori alle variabili. Ad esempio, se il nostro file di testo contiene due numeri separati da una virgola, possiamo leggerli e assegnarli a due variabili `x` e `y` utilizzando il seguente codice:

```C
fscanf(fp, "%d,%d", &x, &y);
```

## Approfondimento
Oltre alle funzioni sopra menzionate, ci sono altre opzioni per leggere un file di testo in linguaggio C. Ad esempio, puoi utilizzare la funzione `getc` per leggere carattere per carattere, oppure `fread` per leggere un blocco di byte. Inoltre, è importante gestire gli errori durante la lettura del file, ad esempio controllando il valore restituito dalle funzioni e chiudendo correttamente il file alla fine dell'operazione.

## Vedi Anche
- Documentazione ufficiale sulle funzioni di lettura dei file in C: https://www.tutorialspoint.com/c_standard_library/c_function_fread.htm
- Esempi di codice per leggere e scrivere file di testo in C: https://www.programiz.com/c-programming/c-file-input-output