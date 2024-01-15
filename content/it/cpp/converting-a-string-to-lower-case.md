---
title:                "Trasformare una stringa in minuscolo"
html_title:           "C++: Trasformare una stringa in minuscolo"
simple_title:         "Trasformare una stringa in minuscolo"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché
Se stai lavorando con strumenti di analisi dei dati o stai sviluppando un'applicazione web, potresti dover convertire una stringa in minuscolo per confrontare i dati in modo sicuro e coerente.

##Come fare
```C++
#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

// Funzione per convertire una stringa in minuscolo
void toLowerCase(string& str) {
  transform(str.begin(), str.end(), str.begin(), ::tolower);
}

// Esempio di utilizzo
int main() {
  // Definiamo una stringa
  string str = "QUESTA È UNA STRINGA IN MAIUSCOLO";

  toLowerCase(str);

  // Stampa della stringa in minuscolo
  cout << str << endl;

  return 0;
}

// Output: questa è una stringa in maiuscolo
```

## Deep Dive
Nel linguaggio di programmazione C++, il caso delle lettere è importante nella comparazione delle stringhe. Ciò significa che una stringa in minuscolo e una in maiuscolo non saranno considerate uguali se confrontate. Per evitare errori di questo tipo, è necessario convertire le stringhe in un unico formato di caso prima di confrontarle. Questo può essere fatto utilizzando la funzione `tolower()` della libreria `<algorithm>`.

## Vedi anche
- [Funzione `tolower()`](https://www.cplusplus.com/reference/cctype/tolower/)
- [Guida su come usare la libreria `<algorithm>`](https://www.tutorialspoint.com/cplusplus/cpp_algorithms.htm)
- [C++ Tutorial - Stringhe](https://www.youtube.com/watch?v=l5fVDflXdFg)