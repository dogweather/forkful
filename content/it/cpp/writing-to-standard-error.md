---
title:                "C++: Scrivere su errore standard"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

##Perché
Scrivere su standard error è un'abilità fondamentale per qualsiasi programmatore C++. Quando un programma produce output, questo di solito viene visualizzato sulla standard output. Tuttavia, ci possono essere situazioni in cui si desidera inviare le informazioni di output su standard error invece, ad esempio quando si vuole distinguere tra i diversi tipi di output o quando si vuole registrare gli errori.

##Come
Per scrivere su standard error in C++, è necessario includere la libreria di input/output `iostream`. Successivamente, si può utilizzare la funzione `cerr` per scrivere un messaggio sull'output di errore. Ecco un esempio:

```C++

#include <iostream>
using namespace std;

int main() {
  // Utilizzo di cerr per scrivere un messaggio di errore
  cerr << "Questo è un messaggio di errore" << endl;
  return 0;
}

```

Questo produrrà l'output "Questo è un messaggio di errore" sulla standard error. È anche possibile utilizzare la sintassi `std::cerr` anziché `cerr`, ma si dovrà utilizzare `std::endl` anziché `endl`.

##Approfondimento
Un concetto importante da capire riguardo a standard error è che è un'operazione di I/O (input/output) non-buffered. Ciò significa che i dati vengono inviati immediatamente e non vengono mantenuti in un buffer come avviene con standard output. Questo è importante da tenere a mente quando si desidera scrivere su standard error all'interno di una funzione che verrà chiamata più volte, in quanto ogni chiamata scriverà immediatamente sull'output di errore senza aspettare che il buffer venga svuotato.

##Vedi anche
- [Documentazione su std::cerr](https://www.cplusplus.com/reference/iostream/cerr/)
- [Differenza tra standard output e standard error](https://www.linuxnix.com/stdout-stderr-stdin-3-basics-linux-io-redirection/)
- [Tutorial su input/output in C++](https://www.tutorialspoint.com/cplusplus/cpp_input_output.htm)