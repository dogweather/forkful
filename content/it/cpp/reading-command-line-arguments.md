---
title:                "Lettura degli argomenti della linea di comando"
html_title:           "C++: Lettura degli argomenti della linea di comando"
simple_title:         "Lettura degli argomenti della linea di comando"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Se stai scrivendo un programma in C++, può essere utile leggere gli argomenti della riga di comando. In questo modo, puoi accedere alle informazioni fornite dall'utente e utilizzarle nel tuo codice.

## Come fare

Per leggere gli argomenti della riga di comando in C++, puoi utilizzare l'array "argv" e il numero di elementi "argc" passati alla funzione "main".

```C++
int main(int argc, char* argv[]) {
  // argc: numero di argomenti passati
  // argv: array di argomenti passati come stringhe
  // argv[0]: nome del programma
  // argv[1], argv[2], ...: argomenti aggiuntivi
}
```

Considera questo semplice esempio:

```C++
// Nome del programma: esempio
// Argomenti passati: esempio arg1 arg2

#include <iostream>

int main(int argc, char* argv[]) {
  std::cout << "Numero di argomenti: " << argc << std::endl; // Output: 3
  std::cout << "Nome del programma: " << argv[0] << std::endl; // Output: esempio
  std::cout << "Argomenti aggiuntivi: " << argv[1] << ", " << argv[2]; // Output: arg1, arg2

  return 0;
}
```

Puoi anche utilizzare un ciclo "for" per accedere a tutti gli argomenti della riga di comando.

```C++
for (int i = 0; i < argc; i++) {
  std::cout << "Argomento " << i << ": " << argv[i] << std::endl;
}
```

## Approfondimento

Oltre ai nomi dei file o alle opzioni del programma, puoi anche passare altri tipi di informazioni come argomenti della riga di comando. Ad esempio, puoi utilizzare gli argomenti per specificare le dimensioni di un array o il numero di volte che un'operazione deve essere eseguita.

Tieni presente che gli argomenti della riga di comando sono sempre passati come stringhe, quindi se hai bisogno di un tipo specifico (come un intero o un float), dovrai convertire la stringa corrispondente.

## Vedi anche

- [Come accedere agli argomenti della riga di comando in C++](https://www.programiz.com/cpp-programming/library-function/cstdlib/getenv)
- [Docente del corso di C++ per principianti su Udemy](https://www.udemy.com/course/c-plus-plus-per-principianti/)
- [Tutorial sulla lettura degli argomenti della riga di comando in C++](https://www.tutorialspoint.com/cplusplus/cpp_command_line_arguments.htm)