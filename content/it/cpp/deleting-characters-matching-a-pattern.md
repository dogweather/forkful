---
title:                "Eliminazione dei caratteri corrispondenti a un modello"
html_title:           "PowerShell: Eliminazione dei caratteri corrispondenti a un modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un modello"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

Eliminare i caratteri corrispondenti a un modello è il processo di rimozione di specifici elementi di testo da una stringa sulla base di un criterio definito. I programmatori lo fanno per manipolare e pulire i dati, migliorando così la precisione dell'output del codice.

## Come fare:

Per rimuovere un carattere da una stringa in C++, utilizzeremo la funzione `erase()` e `remove()`. Ecco un esempio:

```C++
#include<iostream>
#include<algorithm>

int main(){
    std::string s = "gattobello";
    s.erase(std::remove(s.begin(), s.end(), 't'), s.end());
    std::cout << s << std::endl;
    
    return 0;
}
```

L'output sarà:

```
gabello
```

## Approfondimento

(1) Nell'ambito della programmazione, l'eliminazione di caratteri è stata una necessità fin dall'avvento dei linguaggi di programmazione. In C++, `erase()` e `remove()` offrono un modo diretto e standard per farlo.

(2) Esistono delle alternative. Ad esempio, potresti iterare sulla stringa originale e comporre una nuova stringa senza i caratteri indesiderati.

(3) `remove()` sposta gli elementi da rimuovere alla fine della sequenza e poi ritorna un iteratore indicante il nuovo past-the-end elemento. `erase()` è poi invocata a rimuovere questi elementi extra.

## Vedi anche

1. [Documentazione C++ su string::erase](http://www.cplusplus.com/reference/string/string/erase/)
2. [Documentazione C++ su std::remove](http://www.cplusplus.com/reference/algorithm/remove/)
3. [Stack Overflow: Cosa fa std::remove?](https://stackoverflow.com/questions/347949/how-to-remove-certain-character-from-a-string)