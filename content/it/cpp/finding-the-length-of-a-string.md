---
title:                "Trovare la lunghezza di una stringa"
html_title:           "C++: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Trovare la lunghezza di una stringa è un'operazione molto comune in programmazione, in particolare quando si lavora con stringhe di testo. Sapere la lunghezza di una stringa può aiutarci a gestire meglio la memoria o a effettuare operazioni di manipolazione sui dati.

## Come Fare

Per trovare la lunghezza di una stringa in C++, possiamo utilizzare la funzione "size()" o "length()" disponibile nelle librerie standard del linguaggio.

```C++
#include <iostream>
using namespace std;

int main() {
	string str = "Ciao a tutti!";
	
	// utilizzando la funzione size()
	cout << "La lunghezza della stringa è: " << str.size() << endl;
	
	// utilizzando la funzione length()
	cout << "La lunghezza della stringa è: " << str.length() << endl;
	
	return 0;
}
```

**Output:**

```
La lunghezza della stringa è: 13
La lunghezza della stringa è: 13
```

Entrambe le funzioni restituiscono un valore di tipo "size_t", che rappresenta la lunghezza della stringa in termini di numero di caratteri.

## Approfondimento

È importante notare che la funzione "size()" e "length()" sono equivalenti in termini di funzionalità e prestazioni. La scelta di utilizzare una rispetto all'altra è più una questione di preferenza personale. Tuttavia, è bene sottolineare che "size()" è considerata una funzione più generale, in quanto può essere utilizzata anche per ottenere la dimensione di altre strutture dati come array o vettori.

Inoltre, è importante fare attenzione al tipo di dato restituito dalla funzione. Come già accennato, "size()" e "length()" restituiscono un valore di tipo "size_t", che è un tipo definito dalle librerie standard di C++ ed è generalmente equivalente a "unsigned int" o "unsigned long". Per questo motivo, è buona pratica utilizzare il tipo "size_t" quando si dichiarano variabili destinate a contenere la lunghezza di una stringa.

## Vedi Anche

- [Documentazione ufficiale su "size()" e "length()" in C++](https://en.cppreference.com/w/cpp/string/basic_string/size)
- [Tutorial su stringhe in C++](https://www.geeksforgeeks.org/strings-in-c-2)
- [Esempi di utilizzo di stringhe in C++](https://www.programiz.com/cpp-programming/library-function/string.c_str)