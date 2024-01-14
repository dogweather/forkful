---
title:    "C++: Maiuscolo di una stringa"
keywords: ["C++"]
---

{{< edit_this_page >}}

### Perché ###

Molti programmatori si chiedono perché dobbiamo impegnarci nella capitalizzazione di una stringa. In realtà, ci sono molte situazioni in cui questo può essere utile, ad esempio quando dobbiamo estrarre dei dati da un database o quando vogliamo formattare un input utente in modo uniforme.

### Come fare ###

Per capitalizzare una stringa in C++, dobbiamo utilizzare la funzione "toupper" della libreria "cctype". Di seguito un esempio di codice che mostra come applicare questa funzione a una stringa di input:

```C++
#include <iostream>
#include <cctype>
using namespace std;

int main() {
    string input;
    cout<<"Inserisci una stringa: ";
    getline(cin, input); //acquisisce una stringa di input dall'utente
    for(int i = 0; i < input.length(); i++) {
        input[i] = toupper(input[i]); //applica la funzione toupper a ogni carattere
    }
    cout<<"Stringa in maiuscolo: "<<input<<endl;
    return 0;
}
```

Esempio di output:

```
Inserisci una stringa: Ciao mondo!
Stringa in maiuscolo: CIAO MONDO!
```

### Approfondimento ###

In C++, i caratteri sono rappresentati da numeri interi, in particolare, dalla loro codifica ASCII. Le lettere maiuscole e minuscole differiscono solo di un bit, quindi per convertire un carattere da maiuscolo a minuscolo (o viceversa) dobbiamo semplicemente aggiungere o sottrarre questo bit. La funzione "toupper" fa esattamente questo, prendendo come argomento un carattere e restituendo la sua corrispettiva lettera maiuscola.

Un modo più efficiente di capitalizzare una stringa è quello di utilizzare il puntatore "toupper" anziché una semplice chiamata alla funzione "toupper". In questo modo, il puntatore "toupper" verrà applicato a ogni elemento della stringa senza bisogno di un ciclo for.

### Vedi anche ###

- [Documentazione della funzione "toupper" (in inglese)](https://en.cppreference.com/w/cpp/string/byte/toupper)
- [Tutorial su stringhe in C++ (in italiano)](https://www.ilmioprofessionista.it/fondamenti_cpluscplus_corso_1.html) 
- [Esempi di codice per capire meccanismi di conversione tra maiuscole e minuscole (in inglese)](https://www.techiedelight.com/convert-string-uppercase-lowercase-cpp/)