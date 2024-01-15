---
title:                "Estrazione di sottostringhe"
html_title:           "Gleam: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Se stai programmando in Gleam, probabilmente hai bisogno di manipolare stringhe di testo. Spesso, ci troviamo ad avere la necessità di estrarre porzioni specifiche di una stringa più grande, ad esempio per ottenere un sottoinsieme di caratteri o per trovare una corrispondenza in un pattern. In questo articolo, vedremo come utilizzare la funzione `substring` di Gleam per estrarre diverse parti di una stringa.

## Come Fare

L'estrazione di substrings è un'operazione molto comune durante la programmazione. Fortunatamente, con Gleam è molto semplice. Possiamo utilizzare la funzione predefinita `substring` che accetta tre argomenti: una stringa di input, l'indice di inizio e l'indice di fine della porzione che vogliamo estrarre.

Ecco un esempio di codice che estrae la quarta fino alla sesta lettera di una stringa:

```Gleam
let stringa = "Ciao Mondo!"
let substr = substring(stringa, 3, 6)
```

Il risultato sarà la parola "o Mo". Nota che l'indice di inizio è 3 perché la prima lettera di una stringa ha indice 0, mentre l'indice di fine è 6 perché la funzione `substring` esclude l'ultimo indice specificato.

Possiamo anche specificare un solo indice, in questo caso l'indice di inizio, per estrarre la parte della stringa a partire da quell'indice fino alla fine:

```Gleam
let substr = substring("Hello World!", 6)
```

Il risultato sarà "World!".

Se invece vogliamo ottenere una sottostringa di una determinata lunghezza a partire da un indice specificato, possiamo utilizzare la funzione `subslice`:

```Gleam
let subslice = substring("Programming is fun!", 5, 8)
```

Il risultato sarà "ammi".

## Approfondimento

La funzione `substring` è molto utile per l'estrazione di substrings, ma ci sono alcune cose importanti da tenere a mente. Prima di tutto, se il secondo indice (l'indice di fine) è maggiore della lunghezza della stringa, la funzione restituirà un errore. Inoltre, entrambi gli indici devono essere numeri positivi e l'indice di fine deve essere maggiore dell'indice di inizio.

Un'altra cosa da notare è che la funzione `substring` crea una copia della porzione estratta della stringa, quindi è meglio utilizzarla solo su stringhe relativamente corte per evitare problemi di memoria.

## Vedi Anche

- [Documentazione ufficiale di Gleam](https://gleam.run/documentation/)
- [Repository GitHub di Gleam](https://github.com/gleam-lang/gleam)