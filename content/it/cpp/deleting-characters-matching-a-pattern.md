---
title:                "C++: Eliminare i caratteri che corrispondono a un modello"
simple_title:         "Eliminare i caratteri che corrispondono a un modello"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché
Ci possono essere molte ragioni per cui si potrebbe voler eliminare dei caratteri che corrispondono ad un certo pattern in un programma scritto in C++. Potrebbe essere necessario pulire una stringa di input o rimuovere degli spazi vuoti per una migliore gestione dei dati. In ogni caso, è importante sapere come farlo correttamente per evitare errori e ottenere il risultato desiderato.

## Come fare
Per eliminare i caratteri che corrispondono ad un pattern in C++, l'approccio più comune è utilizzare la funzione `erase` della classe `std::string`. Questa funzione accetta due parametri: un iteratore di inizio e un iteratore di fine. In questo modo, è possibile specificare la porzione di stringa da cui rimuovere i caratteri.

**Esempio 1**: Eliminare tutte le occorrenze di una lettera in una stringa.

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string str = "ciao mondo";
    cout << "Stringa originale: " << str << endl;
    
    auto iter = str.begin();
    while (iter != str.end()) {
        if (*iter == 'o') {
            iter = str.erase(iter);
        } else {
            iter++;
        }
    }

    cout << "Stringa modificata: " << str << endl;

    return 0;
}
```

**Output**:

```
Stringa originale: ciao mondo
Stringa modificata: cia mnd
```

**Esempio 2**: Eliminare tutti i caratteri non numerici da una stringa.

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string str = "C4s4r4";
    cout << "Stringa originale: " << str << endl;
    
    auto iter = str.begin();
    while (iter != str.end()) {
        if (!isdigit(*iter)) {
            iter = str.erase(iter);
        } else {
            iter++;
        }
    }

    cout << "Stringa modificata: " << str << endl;

    return 0;
}
```

**Output**:

```
Stringa originale: C4s4r4
Stringa modificata: 444
```

## Approfondimento
Ci sono diverse opzioni per gestire il pattern da eliminare quando si utilizza la funzione `erase`. È possibile utilizzare funzioni come `find` o `rfind` per trovare l'iteratore corretto, oppure è possibile usare la funzione `remove_if` insieme ad un predicato per rimuovere i caratteri in modo più efficiente.

Inoltre, bisogna prestare attenzione al fatto che la funzione `erase` restituisce un iteratore valido solo se viene utilizzata con una stringa, altrimenti potrebbe causare comportamenti inaspettati.

## Vedi anche
- [La documentazione ufficiale di `std::string::erase`](https://en.cppreference.com/w/cpp/string/basic_string/erase)
- [Come rimuovere spazi vuoti da una stringa in C++](https://www.coderomeos.org/remove-whitespace-from-string-in-cpp/)