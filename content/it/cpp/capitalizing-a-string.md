---
title:                "Maiuscolare una stringa"
html_title:           "C++: Maiuscolare una stringa"
simple_title:         "Maiuscolare una stringa"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Che cos'è e perché: 
Il processo di capitalizzazione di una stringa si riferisce alla trasformazione dei caratteri iniziali di ogni parola in maiuscolo. Questo è comunemente fatto dai programmatori per rendere più leggibile il testo e per seguire le convenzioni di stile dei linguaggi di programmazione come il C++.

# Come fare:
```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  string str = "prova di capitalizzazione";
  for (int i = 0; i < str.length(); i++) {
    str[i] = toupper(str[i]);
  }
  cout << str << endl;
  return 0;
}
```
## Output: "PROVA DI CAPITALIZZAZIONE"

# Approfondimento:
La capitalizzazione delle stringhe è un processo comune nei linguaggi di programmazione ed è spesso utilizzata per una migliore leggibilità del codice. In passato, i computer non erano in grado di riconoscere le lettere maiuscole e minuscole, quindi l'utilizzo di capitali era un modo per distinguere tra parole diverse. Tuttavia, oggi questo non è più necessario e si tratta più di una convenzione di stile. Alternativamente, è possibile utilizzare una funzione di formattazione delle stringhe come "capitalize" per ottenere lo stesso risultato.

# Vedi anche:
- [Funzione "capitalize" in C++](https://www.cplusplus.com/reference/string/string/capitalize/)
- [Convenzioni di stile in C++](https://en.wikipedia.org/wiki/C++_Core_Guidelines)