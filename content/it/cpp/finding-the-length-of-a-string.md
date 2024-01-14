---
title:                "C++: Calcolare la lunghezza di una stringa"
simple_title:         "Calcolare la lunghezza di una stringa"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Trovare la lunghezza di una stringa è un'operazione molto comune e importante nella programmazione. Conoscere la lunghezza di una stringa consente di gestire meglio le operazioni di elaborazione dei dati, come la ricerca e l'aggiornamento dei dati all'interno della stringa.

## Come Fare

Per trovare la lunghezza di una stringa in C++, possiamo utilizzare la funzione `length()` o `size()`, entrambe disponibili nella libreria standard `string`. Ecco un esempio di come utilizzare queste due funzioni:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
	string nome = "Giorgio";
	int lunghezza = nome.length();

	cout << "La lunghezza della stringa 'nome' è: " << lunghezza << endl;

	string cognome = "Rossi";
	int size = cognome.size();

	cout << "La lunghezza della stringa 'cognome' è: " << size << endl;

	return 0;
}
```

E questo è l'output che otterremo:

```
La lunghezza della stringa 'nome' è: 7
La lunghezza della stringa 'cognome' è: 5
```

## Approfondimento

La funzione `length()` e `size()` restituiscono la stessa cosa, cioè la lunghezza della stringa. Ma qual è la differenza tra di loro? In realtà, sono entrambe chiamate dalla stessa funzione `size_type` della libreria `string` e quindi restituiscono lo stesso tipo di valore. La differenza sta nel fatto che `length()` è rappresentato come un metodo della classe `string`, mentre `size()` è una funzione globale. In generale, possiamo usare indifferentemente una delle due funzioni a seconda delle nostre preferenze.

## Vedi anche
- Documentazione ufficiale di string in C++: https://www.cplusplus.com/reference/string/
- Altro approfondimento sulla libreria string in C++: https://www.geeksforgeeks.org/string-class-in-cpp/