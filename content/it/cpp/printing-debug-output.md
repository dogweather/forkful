---
title:                "Stampa dell'output di debug"
html_title:           "C++: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Stampare l'output di debug è un'attività comune per i programmatori C++. Questa tecnica aiuta a identificare e risolvere errori nel codice in modo più efficiente.

## Come Farlo

Per stampare un output di debug in C++, utilizzeremo la funzione `cout` dall'header `<iostream>`. Questa funzione ci permette di stampare messaggi su standard output. Vediamo un esempio di codice:

```C++
#include <iostream>

int main() {
  std::cout << "Questo è un messaggio di debug" << std::endl;
  return 0;
}
```

Questo codice stamperà il messaggio "Questo è un messaggio di debug" seguito da una nuova riga su standard output. Si può anche utilizzare la funzione `cerr` per stampare su standard error. Ecco un altro esempio:

```C++
#include <iostream>

int main() {
  std::cerr << "Errore: variabile non inizializzata" << std::endl;
  return 0;
}
```

Questo codice stamperà il messaggio di errore "Errore: variabile non inizializzata" su standard error.

## Deep Dive

È importante notare che la stampa di output di debug dovrebbe essere una pratica temporanea e non dovrebbe essere inclusa nel codice finale che verrà distribuito agli utenti. Invece, si può utilizzare la compilazione condizionale per includere o escludere l'output di debug, ad esempio utilizzando `#ifdef` e `#endif`:

```C++
#include <iostream>

#define DEBUG
// Si può anche utilizzare #define NDEBUG per disabilitare output di debug
// prima di includere l'header <cassert> in cui dichiarare la macros assert().

int main() {
  
#ifdef DEBUG
  std::cout << "Output di debug" << std::endl;
#endif
  
  // Altro codice
  
  return 0;
}
```

Oltre alla semplice stampa di messaggi di testo, si può anche utilizzare `cout` per stampare il valore di variabili o espressioni durante l'esecuzione del programma:

```C++
#include <iostream>

int main() {
  int var = 5;
  std::cout << "Valore di var: " << var << std::endl;
  
  int result = var * 2;
  std::cout << "Risultato: " << result << std::endl;
  
  return 0;
}
```

Questo codice stamperà "Valore di var: 5" e "Risultato: 10" su standard output.

## Vedi Anche

- [Informazioni su output di debug in C++](https://www.geeksforgeeks.org/types-of-output-of-a-cpp-program)
- [Guida dettagliata alla gestione degli errori in C++](https://www.tutorialspoint.com/cplusplus/cpp_exceptions_handling.html)
- [Utilizzo dei compilatori condizionali in C++](https://en.cppreference.com/w/cpp/preprocessor/conditional)