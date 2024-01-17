---
title:                "Scrivere su standard error"
html_title:           "C++: Scrivere su standard error"
simple_title:         "Scrivere su standard error"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

Cosa e perché?
Scrivere sulla standard error è un modo utile per i programmatori per comunicare con i loro utenti attraverso il terminale del sistema operativo. Questo è particolarmente utile quando si vogliono mostrare errori o messaggi informativi durante l'esecuzione del programma.

Come fare:
Nella programmazione in C++, è possibile scrivere sulla standard error utilizzando la funzione "cerr" o "std::cerr". Ciò consente di stampare messaggi sulla finestra del terminale, invece che sulla standard output. Ecco un esempio di come fare:

```C++
#include <iostream>

int main() {
  std::cerr << "Questo è un messaggio di errore!" << std::endl;
  return 0;
}
```

Output:
```
Questo è un messaggio di errore!
```

Deep Dive:
Scrive sulla standard error ha una lunga storia nella programmazione. Prima dell'avvento dei sistemi operativi moderni, i programmatori spesso utilizzavano la standard error per comunicare con gli utenti e avvisarli di eventuali errori durante l'esecuzione dei loro programmi.

Una delle alternative più comuni per scrivere sulla standard error è utilizzare la funzione "fprintf" della libreria standard di C. Questo consente di scrivere su qualsiasi file aperto, compresa sia la standard output che la standard error.

Per quanto riguarda l'implementazione, scrivere sulla standard error funziona allo stesso modo della standard output, ma richiede l'utilizzo della funzione "cerr" o "std::cerr".

Vedi anche:
- [La funzione "fprintf" di C](https://www.cplusplus.com/reference/cstdio/fprintf/)
- [Informazioni sulle standard streams in C++](https://www.learncpp.com/cpp-tutorial/standard-input-output-cout-cin-and-cerr/)
- [Istruzioni di Microsoft su "printf" e "fprintf"](https://docs.microsoft.com/en-us/cpp/c-runtime-library/crt-functions/fprintf-fprintf-l-printw-printw-l?view=msvc-160)