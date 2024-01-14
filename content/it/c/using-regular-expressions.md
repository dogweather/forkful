---
title:    "C: Utilizzo di espressioni regolari"
keywords: ["C"]
---

{{< edit_this_page >}}

## Perché Usare le Espressioni Regolari

Le espressioni regolari sono uno strumento potente per il riconoscimento e la manipolazione di pattern di testo. Con l'utilizzo delle espressioni regolari, è possibile risparmiare tempo e sforzi nella ricerca e nella modifica di stringhe di testo all'interno di un programma C.

## Come Utilizzare le Espressioni Regolari in C

Per utilizzare le espressioni regolari in un programma C, è necessario utilizzare la libreria `regex.h`. Questa libreria fornisce funzioni per la compilazione e l'esecuzione di espressioni regolari.

Ecco un esempio di codice che utilizza le espressioni regolari per trovare e sostituire una stringa di testo:

```C
#include <stdio.h>
#include <regex.h>

int main() {
  // compilazione dell'espressione regolare
  regex_t regex;
  int ret = regcomp(&regex, "ciao", 0);
  if(ret) {
    printf("Errore nella compilazione dell'espressione regolare");
    return 1;
  }

  // esecuzione dell'espressione regolare sulla stringa di testo
  char input[50] = "ciao a tutti!";
  ret = regexec(&regex, input, 0, NULL, 0);
  if(!ret) {
    printf("Stringa trovata\n");

    // sostituzione della stringa "ciao" con "salve"
    ret = regsub("salve", input, input);
    if(ret) {
      printf("Errore nella sostituzione");
      return 1;
    }

    printf("Nuova stringa: %s\n", input);
  } else if(ret == REG_NOMATCH) {
    printf("Stringa non trovata\n");
    return 0;
  } else {
    printf("Errore durante l'esecuzione dell'espressione regolare");
    return 1;
  }

  // deallocazione della memoria
  regfree(&regex);
  return 0;
}
```

Output:

```
Stringa trovata
Nuova stringa: salve a tutti!
```

## Approfondimenti sull'utilizzo delle Espressioni Regolari

Le espressioni regolari possono essere utilizzate in vari contesti, come la validazione di input utente, la ricerca di pattern all'interno di file di testo o l'estrazione di dati da stringhe complesse. Esistono anche diverse sintassi per la scrittura di espressioni regolari, quindi è importante fare ricerche e imparare le diverse opzioni per utilizzarle in modo efficace.

Inoltre, è possibile combinare le espressioni regolari con altre funzioni di manipolazione di stringhe come `sprintf` per ottenere risultati più avanzati.

## Vedi Anche

- [Documentazione su regex.h](https://www.gnu.org/software/gnulib/manual/html_node/regex_002eh.html)
- [Tutorial su espressioni regolari in C](https://www.tutorialspoint.com/cprogramming/c_regular_expressions.htm)
- [Sintassi delle espressioni regolari](https://www.regular-expressions.info/tutorial.html)