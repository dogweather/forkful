---
title:                "C++: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitalize una stringa è un'operazione molto comune in programmazione. Spesso, è necessario trasformare una stringa in maiuscolo per formattare correttamente un output o confrontare input degli utenti indipendentemente dalla capitalizzazione. Utilizzare una funzione per capitalizzare una stringa può semplificare notevolmente il nostro codice e risparmiare tempo nella scrittura di codice ripetitivo.

## Come

Per capitalizzare una stringa in C++, possiamo utilizzare una semplice funzione che converte ogni carattere della stringa in maiuscolo. Ecco un esempio di codice che mostra come utilizzare questa funzione:

```C++
#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

string capitalizza(string str) {
    // Utilizziamo la funzione transform per convertire tutti i caratteri della stringa in maiuscolo
    transform(str.begin(), str.end(), str.begin(), ::toupper);
    return str;
}

int main() {
    string str = "ciao a tutti";
    cout << capitalizza(str) << endl;
    // Output: CIAO A TUTTI
    return 0;
}
```

In questo esempio, abbiamo incluso la libreria `string` per lavorare con le stringhe e la libreria `algorithm` per utilizzare la funzione `transform`. La funzione `capitalizza` prende una stringa come input, utilizza la funzione `transform` per convertire tutti i caratteri in maiuscolo e restituisce la stringa risultante. Possiamo poi utilizzare questa funzione in altre parti del nostro codice dove vogliamo che la stringa sia in maiuscolo.

## Deep Dive

Capire come funziona la funzione `capitalizza` ci può aiutare ad utilizzarla in modo più efficace. Come accennato prima, la funzione `transform` prende in input tre parametri: gli iteratori `begin` e `end` che indicano l'inizio e la fine della porzione di stringa su cui devono essere utilizzati la funzione, e una funzione o una lambda expression che specifica come convertire ogni carattere. Nell'esempio usiamo `::toupper` per convertire ogni carattere in maiuscolo, ma possiamo anche scrivere una funzione personalizzata per ottenere un diverso comportamento.

Inoltre, è importante notare che la funzione `transform` non modifica la stringa originale, ma ne restituisce una nuova. Per questo motivo, è necessario assegnare il risultato della funzione ad una nuova variabile o sovrascrivere la variabile originale con il nuovo valore.

## Vedi anche

- [std::string in C++](https://www.cplusplus.com/reference/string/string/)
- [std::transform in C++](https://www.cplusplus.com/reference/algorithm/transform/)