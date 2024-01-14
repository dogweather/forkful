---
title:    "C: Scrivere su standard error"
keywords: ["C"]
---

{{< edit_this_page >}}

## Perché

Scrivere alla standard error in C può sembrare una pratica poco comune e forse non molto utile a prima vista. Tuttavia, imparare a farlo può essere un'abilità estremamente utile, specialmente quando si lavora su progetti più complessi o quando si desidera visualizzare informazioni di errore specifiche e dettagliate durante la fase di debugging.

## Come Fare

Il processo di scrittura alla standard error in C è abbastanza semplice e può essere realizzato utilizzando la funzione `fprintf()` dalla libreria `<stdio.h>`. Ecco un esempio di codice che mostra come scrivere una stringa alla standard error:

```
#include <stdio.h>

int main() {
  fprintf(stderr, "Questo è un messaggio di errore!\n");
  return 0;
}
```

L'output sarà il seguente:

```
Questo è un messaggio di errore!
```

In questo esempio, `fprintf()` viene utilizzato per scrivere una stringa alla standard error anziché alla standard output (stampa a schermo), come si farebbe normalmente con la funzione `printf()`.

## Approfondimento

Scrivere alla standard error è particolarmente utile in situazioni in cui è necessario visualizzare informazioni di errore più specifiche rispetto a quanto si otterrebbe utilizzando la funzione `perror()` (che scrive un messaggio di errore standard dalla stringa relativa all'ultimo errore) o `strerror()` (che restituisce una descrizione di un errore specifico).

Un altro vantaggio di utilizzare `fprintf()` per scrivere alla standard error è che si può decidere di scrivere in qualsiasi momento durante l'esecuzione del programma, a differenza della funzione `perror()` che scrive solo alla fine del programma.

## Vedi Anche

- [Documentazione su fprintf()](https://www.cplusplus.com/reference/cstdio/fprintf/)
- [Spiegazione di stderr](https://stackoverflow.com/questions/10522012/why-is-stderr-used)
- [Esempi di utilizzo di fprintf()](https://www.geeksforgeeks.org/fprintf-in-c/)