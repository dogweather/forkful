---
title:                "C++: Unione di stringhe"
simple_title:         "Unione di stringhe"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

La concatenazione di stringhe è un'operazione comune nella programmazione C++. Si utilizza per unire due o più stringhe in una sola, creando così una nuova stringa. Questa tecnica è utile quando si desidera creare dinamicamente un output che includa le informazioni dalle stringhe esistenti, come ad esempio un messaggio di benvenuto personalizzato per un utente.

## Come Fare

La concatenazione di stringhe in C++ è un'operazione semplice che coinvolge l'utilizzo dell'operatore "+" o dell'operatore di assegnazione "+=". Ecco un esempio di codice che utilizza l'operatore "+":

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  string nome = "Paola";
  string cognome = "Rossi";
  string messaggio = "Benvenuta " + nome + " " + cognome + "!";
  cout << messaggio << endl;
  return 0;
}
```

Output:
```
Benvenuta Paola Rossi!
```

Ecco invece un esempio di codice che utilizza l'operatore "+=":

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  string nome = "Giuseppe";
  string messaggio = "Ciao ";
  messaggio += nome;
  messaggio += ", benvenuto!";
  cout << messaggio << endl;
  return 0;
}
```

Output:
```
Ciao Giuseppe, benvenuto!
```

## Approfondimento

Esistono alcune considerazioni importanti da tenere a mente quando si concatenano stringhe in C++. In primo luogo, è necessario che il tipo delle stringhe da concatenare sia lo stesso. Ad esempio, non è possibile concatenare una stringa con un intero senza prima convertire l'intero in una stringa. Inoltre, è importante prestare attenzione all'utilizzo delle virgolette quando si concatenano stringhe. Esse non devono essere utilizzate prima della prima stringa o dopo l'ultima, altrimenti potrebbe verificarsi un errore di compilazione.

## Vedi Anche

- [C++ Reference - Operatore di concatenazione stringa (+)](https://www.cplusplus.com/reference/string/string/operator+/)
- [C++ Reference - Operatore di assegnazione con concatenazione stringa (+=)](https://www.cplusplus.com/reference/string/string/operator+=/)
- [Tutorialspoint - Concatenazione stringhe in C++](https://www.tutorialspoint.com/cplusplus/cpp_strings_concatenation.htm)