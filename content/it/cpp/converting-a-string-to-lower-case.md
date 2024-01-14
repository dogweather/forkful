---
title:    "C++: Convertire una stringa in minuscolo"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Perché
Se vuoi manipolare e analizzare una stringa in modo più semplice, uno dei passaggi fondamentali è convertirla in minuscolo. Ciò ti permette di effettuare confronti tra stringhe senza dover considerare anche le maiuscole.

## Come Fare
```c++
#include <iostream>
#include <string>
#include <locale>

using namespace std;

int main()
{
	string str = "QUESTA E' UNA STRINGA IN MAIUSCOLO";
	cout << "Stringa originale: " << str << endl;

	// Converting string to lower case
	locale loc;
	for (int i = 0; i < str.size(); i++)
	{
		str[i] = tolower(str[i], loc);
	}
	cout << "Stringa convertita in minuscolo: " << str << endl;

	return 0;
}
```

**Output:**
```
Stringa originale: QUESTA E' UNA STRINGA IN MAIUSCOLO
Stringa convertita in minuscolo: questa e' una stringa in maiuscolo
```

## Deep Dive
La funzione `tolower()` converte una singola lettera in un carattere minuscolo, utilizzando la localizzazione impostata. Per convertire una stringa intera, è necessario ciclare attraverso tutti i caratteri e applicare la funzione `tolower()` a ognuno di essi. Inoltre, è necessario definire una variabile di tipo `locale` per indicare la localizzazione corrente, altrimenti la conversione potrebbe non essere corretta per alcune lingue.

## Vedi Anche
- [Funzione `tolower()` in C++](https://www.cplusplus.com/reference/cctype/tolower/)
- [Come convertire una stringa in maiuscolo in C++](https://www.linkedin.com/pulse/come-convertire-una-stringa-in-maiuscolo-c-matteo-pessina/)
- [Utilizzo delle localizzazioni in C++](https://www.cplusplus.com/reference/locale/)