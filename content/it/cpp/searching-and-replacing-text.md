---
title:    "C++: Ricerca e sostituzione di testo"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Cercare e Sostituire Testo in C++

## Perché

La ricerca e la sostituzione di testo sono due operazioni comuni nei programmi informatici. Queste operazioni permettono di modificare rapidamente e facilmente grandi quantità di testo, risparmiando tempo e sforzo.

## Come Fare

Per effettuare la ricerca e la sostituzione di testo in C++, è possibile utilizzare le funzioni della libreria string come `find()` e `replace()`. Di seguito è riportato un esempio di come utilizzare queste funzioni:

```
#include <iostream>
#include <string>

using namespace std;

int main() {
    // Dichiarazione di una stringa
    string frase = "Ciao a tutti, benvenuti al mio blog!";
    cout << "Frase originale: " << frase << endl;
    
    // Ricerca della parola "blog" all'interno della stringa 
    size_t pos = frase.find("blog");
    
    // Sostituzione della parola con "sito web"
    if (pos != string::npos) { // Controllo se la parola è stata trovata
        frase.replace(pos, 4, "sito web");
    }
    
    cout << "Frase modificata: " << frase << endl;
    
    return 0;
}

```
L'output di questo codice sarà:

```
Frase originale: Ciao a tutti, benvenuti al mio blog!
Frase modificata: Ciao a tutti, benvenuti al mio sito web!
```

## Approfondimenti

Per eseguire ricerche e sostituzioni più complesse, è possibile utilizzare espressioni regolari. Queste permettono di specificare un pattern che deve essere cercato all'interno del testo e di sostituirlo con un'altra stringa. Per utilizzare le espressioni regolari in C++, è necessario includere la libreria `<regex>` e utilizzare le funzioni `regex_search()` e `regex_replace()`. Di seguito è un esempio di questo approccio:

```
#include <iostream>
#include <string>
#include <regex>

using namespace std;

int main() {
    // Dichiarazione di una stringa
    string testo = "Il 15/03/2021 ho comprato un nuovo telefono.";
    cout << "Testo originale: " << testo << endl;
    
    // Ricerca di una data all'interno del testo
    regex data("([0-9]{2})/([0-9]{2})/([0-9]{4})");
    smatch match;
    
    if (regex_search(testo, match, data)) { // Controllo se la data è stata trovata
        string giorno = match[1];
        string mese = match[2];
        string anno = match[3];
        
        // Sostituzione con la data nel formato giorno-mese-anno
        string nuovaData = match.format("$3-$2-$1");
        testo = regex_replace(testo, data, nuovaData);
    }
    
    cout << "Testo modificato: " << testo << endl;
    
    return 0;
}

```
L'output di questo codice sarà:

```
Testo originale: Il 15/03/2021 ho comprato un nuovo telefono.
Testo modificato: Il ho comprato un nuovo telefono.
```

## Vedi Anche

- ["C++ String replace()" - GeeksforGeeks](https://www.geeksforgeeks.org/cpp-string-replace/)
- ["C++ Regex Tutorial" - CppReference](https://en.cppreference.com/w/cpp/regex)
- ["C++ Regular Expression (regex)" - Programiz](https://www.programiz.com/cpp-programming/regex)