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

## Perché

Scrivere su standard error (stderr) è utile quando si vuole fornire informazioni di debug o errori durante l'esecuzione di un programma, senza influenzare l'output del programma stesso.

## Come fare

Per scrivere su stderr in C++, è necessario includere la libreria ```<iostream>``` e utilizzare l'operatore ```<<``` per inserire il messaggio desiderato seguito da ```std::cerr```. Di seguito un esempio di codice:

```C++
#include <iostream>

int main() {
  int numero = 0;
  if (numero == 0) {
    std::cerr << "Errore: il numero deve essere diverso da zero!\n";
    return 1;
  }
  return 0;
}
```

L'output di questo programma sarà "Errore: il numero deve essere diverso da zero!", stampato su stderr.

## Approfondimento

Standard error è uno dei tre canali di output disponibili in C++. Gli altri due sono standard output (stdout) e standard log (stdlog). A differenza di stdout, il contenuto di stderr viene stampato sullo schermo anche se il programma è in fase di redirect, a meno che non si specifichi diversamente.

## Vedi anche

- Guida completa a standard output e input in C++: [link al tutorial](https://www.learncpp.com/cpp-tutorial/186-basic-standard-output-and-input-cout-cin/)
- Come gestire gli errori in C++: [link all'articolo](https://www.geeksforgeeks.org/error-handling-c/)
- Documentazione ufficiale di C++: [link al sito](https://en.cppreference.com/w/)