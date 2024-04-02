---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:17.720316-07:00
description: "Gli array associativi, noti come `std::map` o `std::unordered_map` in\
  \ C++, colmano il divario tra gli indici degli array e i dati del mondo reale,\u2026"
lastmod: '2024-03-13T22:44:43.719423-06:00'
model: gpt-4-0125-preview
summary: "Gli array associativi, noti come `std::map` o `std::unordered_map` in C++,\
  \ colmano il divario tra gli indici degli array e i dati del mondo reale,\u2026"
title: Utilizzo di array associativi
weight: 15
---

## Cosa e perché?

Gli array associativi, noti come `std::map` o `std::unordered_map` in C++, colmano il divario tra gli indici degli array e i dati del mondo reale, consentendoti di usare chiavi significative. Sono la scelta prediletta quando hai bisogno di ricerche, inserimenti e cancellazioni rapide utilizzando chiavi piuttosto che posizioni di indice.

## Come fare:

In C++, gli array associativi prendono vita con gli header `<map>` e `<unordered_map>`. Spezziamo in esempi per vedere entrambi in azione.

### Usare `std::map`

`std::map` mantiene gli elementi ordinati in base alla chiave. Ecco come iniziare:

```C++
#include <iostream>
#include <map>
#include <string>

int main() {
    std::map<std::string, int> ageMap;
    
    // Inserire valori
    ageMap["Alice"] = 30;
    ageMap["Bob"] = 25;
    
    // Accedere ai valori
    std::cout << "Età di Bob: " << ageMap["Bob"] << std::endl;
    
    // Iterare su una map
    for(const auto &pair : ageMap) {
        std::cout << pair.first << " ha " << pair.second << " anni." << std::endl;
    }
    
    return 0;
}
```

### Usare `std::unordered_map`

Quando l'ordine non è importante, ma lo è la performance, `std::unordered_map` è il tuo amico, offrendo una complessità media più rapida per inserimenti, ricerche e cancellazioni.

```C++
#include <iostream>
#include <unordered_map>
#include <string>

int main() {
    std::unordered_map<std::string, double> productPrice;
    
    // Inserire valori
    productPrice["milk"] = 2.99;
    productPrice["bread"] = 1.99;
    
    // Accedere ai valori
    std::cout << "Prezzo del latte: $" << productPrice["milk"] << std::endl;
    
    // Iterare su una unordered_map
    for(const auto &pair : productPrice) {
        std::cout << pair.first << " costa $" << pair.second << std::endl;
    }
    
    return 0;
}
```

## Approfondimento

Gli array associativi in C++, in particolare `std::map` e `std::unordered_map`, non sono solo per l'archiviazione degli elementi. Forniscono una base per una gestione dei dati più complessa permettendo operazioni come ricerca, inserimento e rimozione in tempi di complessità efficienti (logaritmici per `std::map` e tempo medio costante per `std::unordered_map`). Questa efficienza deriva dalle strutture di dati sottostanti: un albero bilanciato per `std::map` e una tabella hash per `std::unordered_map`.

Storicamente, prima che fossero parte della libreria standard, i programmatori dovevano implementare le proprie versioni o utilizzare librerie di terze parti, portando a inconsistenze e potenziali inefficienze. L'inclusione delle mappe nella libreria standard del C++ non solo ha standardizzato il loro uso, ma le ha anche ottimizzate per la performance attraverso diversi compilatori e piattaforme.

Sebbene entrambi siano potenti, la scelta tra un `std::map` e un `std::unordered_map` dipende dalle specificità del tuo caso d'uso. Hai bisogno di dati ordinati e non ti importa di un leggero compromesso sulla performance? Scegli `std::map`. Se cerchi velocità e l'ordine non ti interessa, `std::unordered_map` è probabilmente la scelta migliore.

Tuttavia, è importante notare che quando si lavora con strutture dati complesse, ci sono sempre dei compromessi. In alcuni casi di nicchia, altre strutture dati o persino librerie di terze parti potrebbero offrire prestazioni migliori o funzionalità adatte alle tue esigenze specifiche. Valuta sempre le tue opzioni in base ai requisiti del tuo progetto.
