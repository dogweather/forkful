---
title:                "Ricerca e sostituzione di testo"
html_title:           "C++: Ricerca e sostituzione di testo"
simple_title:         "Ricerca e sostituzione di testo"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Cercare e sostituire del testo è un'operazione molto comune nell'ambito della programmazione. Può essere utile per correggere errori, aggiornare informazioni o semplicemente modificare parti di un programma. Inoltre, ci consente di risparmiare tempo e di automatizzare processi ripetitivi.

## Come fare

Per cercare e sostituire del testo in C++, utilizzeremo la funzione `replace` della libreria standard di C++. Questa funzione ci permette di specificare una porzione di testo da cercare e una da sostituire e poi di applicare la sostituzione a un'intera stringa.

Di seguito un esempio di come utilizzare la funzione `replace`:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  // Definiamo una stringa di esempio
  string frase = "Ciao mondo!";

  // Sostituiamo la parola "mondo" con "programmazione"
  frase.replace(frase.find("mondo"), 5, "programmazione");

  // Stampiamo la stringa modificata
  cout << frase << endl;

  return 0;
}
```

Questo esempio produrrà l'output:

```
Ciao programmazione!
```

## Approfondimento

La funzione `replace` accetta tre parametri: la posizione da cui inizia la sostituzione, la lunghezza della porzione di testo da sostituire e il testo di sostituzione. Tuttavia, possiamo anche utilizzare la funzione `replace` con un quarto parametro opzionale che indica il numero massimo di sostituzioni da effettuare.

Ad esempio, possiamo utilizzare questo quarto parametro per specificare che vogliamo sostituire solo la prima occorrenza di una parola all'interno di una stringa. In questo modo, se la stessa parola compare più volte, verrà sostituita solo la prima volta.

Per maggiori informazioni sulla funzione `replace` e su come gestire le sostituzioni multiple, è possibile fare riferimento alla documentazione ufficiale di C++.

## Vedi anche

- [COIPL - Sostituzione di stringhe in C++](https://www.cplusplus.com/coipl.org/topic.asp?TOPIC_ID=395)
- [Funzione replace della libreria standard di C++](https://en.cppreference.com/w/cpp/string/basic_string/replace)