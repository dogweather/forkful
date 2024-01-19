---
title:                "Utilizzo delle espressioni regolari"
html_title:           "Bash: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Uso delle espressioni regolari in C

## Cos'è & Perché?
Le espressioni regolari (regex) sono uno strumento potente usato per trovare modelli specifici in una stringa. Questi sono molto utilizzati dai programmatori per validare, cercare, sostituire e dividere stringhe.

## Come Fare:
Le espressioni regolari in C possono essere utilizzate con l'aiuto della libreria "regex.h". Qui c'è un semplice esempio:

```C
#include <regex.h> 

int main() 
{ 
    regex_t regex; 
    int risultato; 
    risultato = regcomp(&regex, "[a-z]", 0); 
    risultato = regexec(&regex, "sample", 0, NULL, 0); 

    if (!risultato) 
        printf("Pattern trovato.\n"); 
    else if (risultato == REG_NOMATCH) 
        printf("Pattern non trovato.\n"); 

    regfree(&regex); 
    return 0; 
} 
```
Quando esegui questo codice, l'output sarà: 'Pattern trovato'.

## Approfondimenti
1. Storia: Le espressioni regolari sono nate negli anni ‘60 per l'elaborazione del linguaggio naturale. Oggi sono molto utilizzate sia in linguaggi di scripting che in linguaggi di programmazione come C.
2. Alternative: Sebbene le regex siano molto potenti, l'uso eccessivo può rendere il codice complesso e difficile da mantenere. È sempre buona pratica usare metodi alternativi quando possibile, come le funzioni di stringa integrate.
3. Implementazione: Implementare le regex in C richiede l'uso della libreria "regex.h". Le funzioni principali sono 'regcomp' per compilare l'espressione regolare e 'regexec' per eseguire la corrispondenza.

## Vedi anche:
1. [Libro di O'Reilly "Mastering Regular Expressions"](http://shop.oreilly.com/product/9780596528126.do)
2. [Tutorial di Regular Expressions in C/C++](https://www.geekhideout.com/urlcode.shtml)
3. [Documentazione ufficiale di GNU su regular expressions](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
4. [Esercizi interattivi di RegexOne](https://regexone.com/)