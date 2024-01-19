---
title:                "Ricerca e sostituzione del testo"
html_title:           "Arduino: Ricerca e sostituzione del testo"
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Cercare e Sostituire il Testo con C++

## Cos'è e Perché?
La ricerca e la sostituzione del testo sono operazioni che intervengono sul contenuto di una stringa, modificandola. Queste sono cruciali nella programmazione per manipolare dati, automatizzare correzioni, semplificare l'input/output, e più in generale, semplificare la gestione dei dati.

## Come fare:

Vediamo un semplice esempio su come cercare e sostituire del testo utilizzando C++. Supponiamo che tu voglia trovar la parola "ciao" nella stringa e sostituirla con "buongiorno".

```C++
#include<bits/stdc++.h>
using namespace std;

int main() {
    string s = "Ciao, come stai?";
    size_t pos = s.find("Ciao");
    if (pos != std::string::npos)
        s.replace(pos, 4, "Buongiorno");

    cout << s;
    return 0;
}
```

L'output sarà:

```C++
Buongiorno, come stai?
```
In questo esempio, abbiamo utilizzato l'istruzione `find` per cercare la parola "Ciao" nella stringa. Se la parola è stata trovata, utilizziamo `replace` per sostituirla con "Buongiorno". 

## Approfondimento

Cercare e sostituire il testo è una pratica antica quanto la programmazione stessa. Nel corso del tempo, i linguaggi di programmazione hanno evoluto vari metodi per eseguire queste operazioni, usando algoritmi di matching di stringhe come KMP (Knuth-Morris-Pratt) o Boyer-Moore.

In C++, in alternativa all'uso di `find` e `replace`, potresti voler utilizzare una espressione regolare (`regex_replace`) del modulo standard `<regex>`. Le espressioni regolari offrono maggiore flessibilità e potenza, ma possono essere più complesse da utilizzare.

Di fondo, l'operazione di ricerca e sostituzione consiste nel localizzare una sottostringa all'interno di una stringa e sostituirla con un'altra. Nella maggior parte dei casi, il metodo `find` troverà l'indice iniziale della sottostringa, mentre `replace` prende l'indice iniziale, la lunghezza della sottostringa e la nuova sottostringa come argomenti.

## Vedi Anche

Per ulteriori informazioni e approfondimenti, consulta le seguenti risorse:

1. [C++ Reference: string::find](http://www.cplusplus.com/reference/string/string/find/)
2. [C++ Reference: string::replace](http://www.cplusplus.com/reference/string/string/replace/)
3. [C++ Reference: regex_replace](http://www.cplusplus.com/reference/regex/regex_replace/)
4. [KMP Algorithm](https://en.wikipedia.org/wiki/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm)
5. [Boyer-Moore Algorithm](https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_string-search_algorithm)