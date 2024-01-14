---
title:    "C: Generazione di numeri casuali"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

Perché Generare Numeri Casuali in C

Se sei un programmatore C, probabilmente sai già che la generazione di numeri casuali è una funzionalità fondamentale per molte applicazioni, come giochi, simulazioni e crittografia. Ma perché dovresti voler generare numeri casuali in C? La risposta è semplice: i numeri casuali aggiungono un elemento di imprevedibilità e casualità alle tue applicazioni, rendendole più interessanti e realistiche.

Come Generare Numeri Casuali in C

La generazione di numeri casuali in C può sembrare un compito complicato, ma in realtà è abbastanza semplice. Iniziamo con un semplice esempio di codice per generare un numero casuale tra 1 e 10.

```
#include <stdio.h> 
#include <stdlib.h> //libreria necessaria per usare la funzione rand()

int main() 
{ 
   int numero; 
   numero = rand()%10 + 1; //genera un numero casuale tra 1 e 10 
   printf("Il numero casuale è: %d", numero); //stampa il numero casuale 
   return 0; 
} 
```

Se eseguiamo questo codice, otterremo un output simile a questo:

`Il numero casuale è: 7`

Ovviamente, il numero puù variare ogni volta che il codice viene eseguito.

Deep Dive: Generazione di Numeri Casuali in C

Ora che abbiamo visto un semplice esempio di come generare un numero casuale in C, vediamo più da vicino come funziona il processo. La funzione `rand()` è responsabile per la generazione dei numeri casuali in C. Questa funzione restituisce un numero intero casuale tra 0 e `RAND_MAX`, una costante definita nella libreria `stdlib.h`. Per controllare il range dei numeri casuali generati, dobbiamo utilizzare l'operatore `%` (modulo). Ad esempio, se vogliamo generare un numero casuale tra 1 e 100, dovremmo utilizzare `rand()%100 + 1`.

Tuttavia, se eseguiamo il codice sopra più volte, noteremo che i numeri casuali generati sono sempre gli stessi. Questo perché la funzione `rand()` utilizza il seme predefinito di 1. Per ottenere numeri realmente casuali ogni volta che eseguiamo il codice, dobbiamo impostare il seme utilizzando la funzione `srand()` e un numero diverso ogni volta. Un metodo comune per fare ciò è utilizzare il valore del tempo di sistema come seme, utilizzando la funzione `time()` della libreria `time.h`.

```
#include <stdio.h> 
#include <stdlib.h> 
#include <time.h> //libreria necessaria per usare la funzione time()

int main() 
{ 
   int numero; 
   srand(time(NULL)); //imposta il seme utilizzando il valore del tempo di sistema 
   numero = rand()%10 + 1; 
   printf("Il numero casuale è: %d", numero); 
   return 0; 
} 
```

Ogni volta che eseguiamo il codice, otterremo un numero casuale differente, rendendo il risultato più verosimile.

See Also

- Guida alla Generazione di Numeri Casuali in C: https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/
- Documentazione sulla Funzione rand(): https://en.cppreference.com/w/c/numeric/random/rand