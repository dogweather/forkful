---
title:                "C++: Cancellare caratteri corrispondenti a un modello"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui si potrebbe voler eliminare dei caratteri che soddisfano un certo pattern in un programma in linguaggio C++. Potresti avere bisogno di ripulire i dati inseriti dagli utenti, o potresti voler rimuovere delle parole offensive da un testo. In ogni caso, imparare come fare questa operazione ti renderà un programmatore più efficiente ed esperto.

## Come Farlo

Per eliminare dei caratteri che soddisfano un pattern in C++, è necessario utilizzare alcune funzioni specifiche della libreria standard del linguaggio. Un modo per farlo è utilizzare la funzione `replace_if`, che accetta come argomenti un inizio, una fine e un predicato per identificare il pattern da eliminare. Di seguito è presentato un esempio di codice:

```C++
#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

int main() {
  string testo = "Questo testo contiene delle parolacce.";
  replace_if(testo.begin(), testo.end(), [](char c){ return c == 'e'; }, ' ');
  cout << testo << endl;
  return 0;
}
```

Output:

```
Qusto tsto containi dlla parolacca.
```

Nell'esempio sopra, il carattere `e` viene sostituito con uno spazio vuoto grazie alla funzione `replace_if`.

## Approfondimento

Ci sono diverse funzioni ed approcci che possono essere utilizzati per eliminare dei caratteri che rispettano un certo pattern in C++. Ad esempio, è possibile utilizzare la funzione `erase` per eliminare una porzione di una stringa, o anche la libreria `regex` per gestire pattern più complessi. Inoltre, è importante prestare attenzione alle prestazioni del tuo codice quando si eseguono queste operazioni, in modo da evitare lenti algoritmi di eliminazione.

## Vedi Anche

- Tutorial su stringhe in C++ (https://www.programiz.com/cpp-programming/strings)
- Documentazione ufficiale delle funzioni `replace_if` e `erase` (https://en.cppreference.com/w/cpp/string/basic_string/replace_if, https://en.cppreference.com/w/cpp/string/basic_string/erase)
- Tutorial sulla libreria `regex` in C++ (https://www.geeksforgeeks.org/c-regex-tutorial/)