---
title:                "Eliminazione dei caratteri corrispondenti a un modello"
html_title:           "PowerShell: Eliminazione dei caratteri corrispondenti a un modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un modello"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Che Cosa e Perché?

Eliminare i caratteri corrispondenti a un pattern significa rimuovere specifici insiemi di caratteri etichettati da un espressione regolare all'interno di una stringa. Questa operazione è spesso necessaria per pulire o formattare i dati di input.

## Come fare:

Ecco un esempio su come poter eliminare caratteri corrispondenti a un pattern usando la funzione Javascript `replace()` combinata con le espressioni regolari.

```Javascript
var str = "Ciao, Mondo!123";
var pattern = /[0-9]/g;
var newStr = str.replace(pattern, "");
console.log(newStr);
```
Il risultato di questo codice sarà:

```
"Ciao, Mondo!"
```
In questo caso, il pattern è `/[0-9]/g` che corrisponde a tutti i numeri (0-9) nella stringa. L'opzione `g` significa "globale", eliminando quindi tutte le occorrenze del pattern e non soltanto la prima.

## Approfondimenti:

Historicamente, il concetto di espressioni regolari deriva dal linguaggio di programmazione Perl, ma è stato successivamente adottato da Javascript e altri linguaggi. Sebbene la funzione `replace()` sia l'approccio più comune per eliminare caratteri corrispondenti a un pattern, potresti anche utilizzare metodi come `split()` e `join()`. Ad esempio:
  
```Javascript
var str = "Ciao, Mondo!123";
var newStr = str.split(/[0-9]/).join("");
console.log(newStr);
```
Questo codice fornisce lo stesso risultato. `split()` divide la stringa in un array usando il pattern come delimitatore, poi `join()` unisce l'array in una stringa.

## Guarda Anche: 

Per approfondire ulteriormente le espressioni regolari in Javascript, è possibile consultare queste risorse:

1. [MDN Regular Expressions](https://developer.mozilla.org/it/docs/Web/JavaScript/Guida/Regular_Expressions): Un'ampia panoramica delle espressioni regolari in Javascript.
2. [RegExr](https://regexr.com/): Uno strumento online per esercitarsi con le espressioni regolari.
3. [RegExp JavaScript](https://www.w3schools.com/jsref/jsref_obj_regexp.asp) su W3School: offre esempi concreti su come utilizzare le RegExp in JavaScript. Il sito è in inglese, ma è molto intuitivo.