---
title:    "C++: Lettura degli argomenti della riga di comando"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Perché

Scrivere un programma che prende in input degli argomenti da linea di comando può sembrare un'operazione banale, ma in realtà può essere molto utile per automatizzare alcune attività o creare programmi più dinamici ed interattivi. Leggendo questo articolo scoprirai come leggere gli argomenti dalla riga di comando e utilizzarli nell'esecuzione del tuo programma.

## Come Fare

Per leggere gli argomenti dalla linea di comando in C++, abbiamo bisogno di utilizzare la funzione `main` e passare due parametri: `argc` (numero di argomenti) e `argv` (array degli argomenti). Ad esempio:

```C++
int main(int argc, char *argv[]) {
  // inserire qui il codice per leggere gli argomenti
}
```

L'argomento `argc` ci permette di sapere quanti argomenti sono stati inseriti mentre `argv` è un array di stringhe contenente i valori di ogni argomento. Possiamo quindi accedere a questi valori utilizzando gli indici dell'array, ad esempio `argv[0]` per il nome del programma e `argv[1]` per il primo argomento.

Proviamo a scrivere un semplice programma che stampa gli argomenti inseriti dall'utente:

```C++
#include <iostream>

int main(int argc, char *argv[]) {
  std::cout << "Hai inserito " << argc << " argomenti:" << std::endl;
  for (int i = 0; i < argc; i++) {
    std::cout << "  - " << argv[i] << std::endl;
  }
}
```

Se eseguiamo questo programma passando come argomenti "C++" e "programming", l'output sarà il seguente:

```bash
$ ./programma C++ programming
Hai inserito 3 argomenti:
  - ./programma
  - C++
  - programming
```

## Approfondimento

In realtà, la gestione degli argomenti da linea di comando è molto più complessa e può diventare una vera e propria sfida nella programmazione in C++. Ad esempio, dobbiamo gestire eventuali errori nell'inserimento degli argomenti o convertirli nel tipo di dato desiderato (poiché vengono sempre passati come stringhe).

Inoltre, può essere utile utilizzare librerie esterne come `boost` che offrono funzionalità avanzate per la gestione dei comandi da riga di comando.

## Vedi Anche

- [Documentazione ufficiale di C++ sui parametri da linea di comando](https://en.cppreference.com/w/cpp/language/main_function)
- [Tutorial su come gestire gli argomenti da linea di comando in C++](https://www.learncpp.com/cpp-tutorial/27-command-line-arguments/)
- [Libreria `boost::program_options` per la gestione avanzata dei comandi da linea di comando](https://www.boost.org/doc/libs/1_75_0/doc/html/program_options.html)