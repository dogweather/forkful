---
title:                "Utilizzare le espressioni regolari"
html_title:           "C++: Utilizzare le espressioni regolari"
simple_title:         "Utilizzare le espressioni regolari"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché

Le espressioni regolari sono uno strumento potentissimo a disposizione dei programmatori, che permettono di ricercare e manipolare pattern di testo in modo rapido ed efficiente. Se si desidera risparmiare tempo e fatica nella gestione di stringhe di caratteri, le espressioni regolari sono un'ottima scelta.

## Come utilizzarle

Per utilizzare le espressioni regolari in C++, è necessario importare la libreria `regex`. Da qui, si può creare un oggetto di tipo `regex` che identifica il pattern da cercare, e utilizzarlo con le funzioni `regex_match` e `regex_search` per trovare corrispondenze all'interno di una stringa.

```C++
#include <iostream>
#include <regex>
using namespace std;

int main() {
    // Pattern per cercare una sequenza di tre cifre
    regex pattern("\\d{3}"); 
    
    // Stringa in cui cercare
    string test = "123 abc 456 def 789 ghi";

    // Verifica se la stringa contiene una corrispondenza esatta con il pattern
    if (regex_match(test, pattern)) { 
        cout << "Corrispondenza trovata!" << endl;
    }

    // Cerca e stampa tutte le corrispondenze trovate nella stringa
    smatch match; // Oggetto che conterrà le corrispondenze
    while (regex_search(test, match, pattern)) {
        cout << "Trovata corrispondenza: " << match.str() << endl;
        test = match.suffix().str(); // Rimuove la corrispondenza trovata dalla stringa
    }
    return 0;
}
```

### Output:

```
Corrispondenza trovata!
Trovata corrispondenza: 123
Trovata corrispondenza: 456
Trovata corrispondenza: 789
```

## Approfondimento

Le espressioni regolari offrono una vasta gamma di potenti strumenti per manipolare e ricercare stringhe di caratteri. Oltre alle opzioni base come `regex_match` e `regex_search`, è possibile utilizzare le funzioni `regex_replace` e `regex_iterator` per effettuare sostituzioni e iterare tra tutte le corrispondenze trovate all'interno di una stringa.

Inoltre, le espressioni regolari possono essere utilizzate per valutare la validità di una stringa rispetto a un particolare pattern, ad esempio per verificare se un indirizzo email è ben formato.

## Vedi anche

- [Documentazione ufficiale di C++ sulle espressioni regolari](https://en.cppreference.com/w/cpp/regex)
- [Tutorial su espressioni regolari in C++](https://www.tutorialspoint.com/cpp_standard_library/cpp_regex.htm)
- [Utilizzo delle espressioni regolari per la validazione di stringhe](https://www.geeksforgeeks.org/how-to-validate-an-email-address-using-regular-expressions-in-cpp/)