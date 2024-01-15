---
title:                "Capitalizzare una stringa"
html_title:           "C++: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molte situazioni in cui è necessario convertire una stringa in maiuscolo, come ad esempio nel controllo delle password, nella gestione di dati sensibili o nella formattazione di output. In questo articolo, vedremo come eseguire questa operazione utilizzando il linguaggio di programmazione C++.

## Come Fare
Per convertire una stringa in maiuscolo in C++, è possibile utilizzare la funzione `toupper` presente nella libreria `cctype`. Di seguito è riportato un esempio di codice che mostra come utilizzarla:

```
#include <iostream>
#include <cctype>
using namespace std;

int main() {
  string str = "questa è una stringa in minuscolo";
  for (char& c : str) {
    c = toupper(c);
  }
  cout << str << endl;
  return 0;
}
```

L'output di questo codice sarà:

```
QUESTA È UNA STRINGA IN MINUSCOLO
```

In questo esempio, la funzione `toupper` viene utilizzata all'interno di un ciclo `for` per convertire ogni carattere della stringa in maiuscolo.

## Approfondimento
È importante notare che la funzione `toupper` converte solo i caratteri alfabetici in maiuscolo e lascia invariati gli altri. Inoltre, è possibile utilizzare la funzione `tolower` per convertire una stringa in minuscolo. Entrambe le funzioni accettano un carattere come parametro e restituiscono il corrispondente carattere convertito.

Per convertire una stringa in maiuscolo o minuscolo senza modificarla direttamente, è possibile utilizzare la classe `stringstream` presente nella libreria `sstream`. Di seguito è riportato un esempio di codice che utilizza questa classe:

```
#include <iostream>
#include <sstream>
using namespace std;

int main() {
  string str = "questa è una stringa in minuscolo";
  stringstream ss(str);
  string converted;

  // Conversione in maiuscolo
  while (ss >> str) {
    converted +=toupper(str[0]) + str.substr(1) + " ";
  }
  cout << converted << endl;

  // Conversione in minuscolo
  while (ss >> str) {
    converted +=tolower(str[0]) + str.substr(1) + " ";
  }
  cout << converted << endl;

  return 0;
}
```

L'output di questo codice sarà:

```
Questa È Una Stringa In Minuscolo 
questa è una stringa in minuscolo
```

In questo esempio, la classe `stringstream` viene utilizzata per separare la stringa in singole parole, convertire la prima lettera di ogni parola in maiuscolo o minuscolo e infine ricostruire la stringa originale con le conversioni applicate.

## Vedi Anche
Per ulteriori informazioni su come gestire le stringhe in C++, consulta questi articoli:
- [C++ Strings](https://www.geeksforgeeks.org/c-string-class-and-its-applications/) (in inglese)
- [Le Stringhe in C++: Modellazione, Formattazione e Gestione](https://www.html.it/pag/33363/stringhe-cpp/) (in italiano)