---
title:                "C++: Cercare e sostituire testo"
simple_title:         "Cercare e sostituire testo"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché
Ciao lettori! Se siete qui, probabilmente siete interessati all'utilizzo del linguaggio di programmazione C++ per cercare e sostituire del testo all'interno del vostro codice. Ebbene, sappiate che questa è una tecnica molto utile per risparmiare tempo e rendere il vostro codice più efficiente.

## Come fare
Per iniziare, dovete comprendere come funziona la ricerca e la sostituzione di testo in C++. Prima di tutto, dovete includere la libreria "string" per lavorare con le stringhe di testo. In seguito, potete utilizzare la funzione "replace" per specificare la stringa che desiderate sostituire, seguita dalla nuova stringa da inserire. Un esempio di codice potrebbe essere il seguente:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  string input = "Ciao a tutti";
  string output = input.replace("a tutti", "ragazzi");

  cout << output << endl;

  return 0;
}
```

In questo caso, la stringa "Ciao a tutti" verrà sostituita con "Ciao ragazzi", che verrà stampata a schermo. Potete giocare con questo codice e testare diverse combinazioni di stringhe da sostituire per comprendere meglio come funziona.

## Approfondimenti
Ora che avete una base solida per la ricerca e la sostituzione di testo in C++, potete approfondire ulteriormente il vostro studio su questa tecnica. Ad esempio, potete imparare a utilizzare la funzione "find" per trovare un determinato testo all'interno di una stringa e sostituirlo solo se viene trovato. Inoltre, potete leggere su altre funzioni utili per la manipolazione di stringhe in C++ come "append" e "substr". Esistono anche librerie esterne che offrono funzionalità avanzate per la ricerca e la sostituzione di testo, quindi non esitate a fare una ricerca e scoprire quali sono più adatte alle vostre esigenze.

## Vedi anche
- [Stringhe in C++](https://www.w3schools.com/cpp/cpp_strings.asp)
- [Manipolazione di stringhe in C++](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)
- [Libreria <string> di C++](https://www.geeksforgeeks.org/string-class-in-cpp/)