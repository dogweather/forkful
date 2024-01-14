---
title:    "C++: Utilizzare le espressioni regolari"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

#Perché usare le espressioni regolari

Le espressioni regolari sono uno strumento potente e versatile per la gestione delle stringhe in un programma C++. Sono particolarmente utili per la ricerca e la manipolazione di testi. Usare le espressioni regolari può aiutare a risparmiare tempo e rendere il codice più leggibile e conciso.

#Come utilizzare le espressioni regolari in C++

Per utilizzare le espressioni regolari in C++, è necessario includere la libreria <regex>. È anche possibile utilizzare gli spazi dei nomi (namespaces) standard std::regex e std::regex_token_iterator per semplificare la sintassi.

Ecco un esempio di codice che utilizza le espressioni regolari per controllare se una stringa contiene un numero di telefono valido:

```C++
#include <regex>
#include <iostream>

using namespace std;

int main() {
    string numero = "345-897-4565";
    
    // Definiamo la nostra espressione regolare
    regex espressione("\\d{3}-\\d{3}-\\d{4}");
    
    // Controlliamo se il numero di telefono è valido
    if (regex_match(numero, espressione)) {
        cout << "Numero di telefono valido!" << endl;
    } else {
        cout << "Numero di telefono non valido." << endl;
    }
    
    return 0;
}
```

Output:

```
Numero di telefono valido!
```

#Approfondimento sulle espressioni regolari

Le espressioni regolari sono costituite da una serie di caratteri che definiscono un modello di ricerca in una stringa. Sono utilizzate per effettuare operazioni di ricerca, sostituzione e validazione dei dati. Possono essere utilizzate anche per gestire testi multilingue e pattern complessi.

Alcuni dei caratteri speciali utilizzati nelle espressioni regolari sono:

- `.` : rappresenta un singolo carattere
- `[]` : rappresenta un gruppo di caratteri
- `^` : indica l'inizio della stringa
- `$` : indica la fine della stringa
- `+` : rappresenta uno o più occorrenze del carattere precedente
- `*` : rappresenta zero o più occorrenze del carattere precedente
- `\d` : rappresenta un carattere numerico

Ci sono molti altri caratteri speciali utilizzati nelle espressioni regolari, quindi è importante consultare la documentazione per una lista completa.

#Vedi anche

- [Tutorial di espressioni regolari in C++](https://www.geeksforgeeks.org/regular-expression-in-c/)
- [Documentazione ufficiale di C++ sulle espressioni regolari](https://en.cppreference.com/w/cpp/regex)
- [Espressioni regolari cheat sheet](https://cheatography.com/davechild/cheat-sheets/regular-expressions/)