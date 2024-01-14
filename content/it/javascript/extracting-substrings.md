---
title:                "Javascript: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

L'estrazione di sottostringhe è un'operazione comune nella programmazione Javascript. Ciò è utile quando si vuole ottenere una porzione specifica di una stringa più grande per utilizzarla in altri calcoli o operazioni.

## Come Fare

Per estrarre una sottostringa, si utilizza il metodo `substring()` di Javascript. Questo metodo richiede due parametri: l'indice di inizio e l'indice di fine della sottostringa da estrarre. Ad esempio, se abbiamo una stringa "Ciao a tutti!" e vogliamo estrarre la parola "tutti", possiamo utilizzare il seguente codice:

```Javascript
let stringa = "Ciao a tutti!";
let sottostringa = stringa.substring(7, 12);
console.log(sottostringa); // Output: tutti
```

Possiamo anche utilizzare `substring()` in combinazione con altri metodi come `indexOf()` per estrarre una sottostringa in base a una parola o carattere specifico. Ad esempio:

```Javascript
let stringa = "Questa è una stringa di esempio.";
let inizio = stringa.indexOf("è"); // Trova l'indice della lettera "è"
let fine = stringa.indexOf(".", inizio); // Trova l'indice del primo punto dopo "è"
let sottostringa = stringa.substring(inizio, fine); // Estrae la sottostringa tra questi due indici
console.log(sottostringa); // Output: è una stringa di esempio
```

## Approfondimento

Il metodo `substring()` accetta anche un solo parametro, il quale specifica l'indice di inizio della sottostringa. In questo caso, verrà estratta la porzione di stringa a partire da questo indice fino alla fine. Ad esempio:

```Javascript
let stringa = "Questo è un esempio di estrazione di sottostringa.";
let sottostringa = stringa.substring(17);
console.log(sottostringa); // Output: un esempio di estrazione di sottostringa.
```

Inoltre, è importante notare che l'indice di fine della sottostringa non è incluso nell'output. Ad esempio, se si utilizza l'indice 6 come parametro di fine, verrà estratta la stringa fino all'indice 5.

## Vedi Anche

- Documentazione ufficiale di ```substring()``` su MDN: https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/substring
- Tutorial su come utilizzare ```substring()``` in combinazione con ```indexOf()```: https://www.w3schools.com/jsref/jsref_substring.asp
- Altri metodi di manipolazione delle stringhe in Javascript: https://www.digitalocean.com/community/tutorials/js-string-manipulation-methods