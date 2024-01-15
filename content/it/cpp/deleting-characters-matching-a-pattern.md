---
title:                "Eliminazione di caratteri che corrispondono a un pattern"
html_title:           "C++: Eliminazione di caratteri che corrispondono a un pattern"
simple_title:         "Eliminazione di caratteri che corrispondono a un pattern"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Ci possono essere diverse ragioni per cui qualcuno potrebbe voler eliminare dei caratteri che corrispondono ad un certo pattern all'interno di una stringa. Potrebbe essere necessario rimuovere informazioni sensibili o inutili, o magari si vuole semplicemente ottenere una stringa più pulita e leggibile.

## Come Fare

Ecco un esempio semplice di come rimuovere tutti i caratteri numerici da una stringa utilizzando la libreria string e la funzione erase.

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
	string input = "123Abc45xyz";
	string output;

	// ciclo sulla stringa di input
	for (int i = 0; i < input.length(); i++) {
		// controllo se il carattere corrente è un numero 
		if (isdigit(input[i])) {
			// se sì, lo elimino dalla stringa di output
			output.erase(i, 1);
		}
	}

	cout << output << endl;
	return 0;
}

```

Output:

```
Abcxyz
```

## Approfondimento

La funzione erase può essere utilizzata per eliminare qualsiasi carattere all'interno di una stringa. È possibile specificare l'indice del carattere da eliminare e il numero di caratteri da eliminare a partire da quel punto. Quindi, se si vuole eliminare tutti i caratteri che corrispondono ad un determinato pattern, basta utilizzare un ciclo e una condizione che controlla il carattere corrente.

È importante notare che la funzione erase modificherà direttamente la stringa di input, quindi è necessario utilizzare una variabile di output per ottenere la stringa senza i caratteri eliminati.

## Vedi Anche

- [Documentazione ufficiale della funzione erase in C++](https://en.cppreference.com/w/cpp/string/basic_string/erase)
- [Esempi di utilizzo della funzione erase](https://www.geeksforgeeks.org/string-erase-function-in-c-stl/)