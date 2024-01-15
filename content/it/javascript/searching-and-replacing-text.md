---
title:                "Ricerca e sostituzione di testo"
html_title:           "Javascript: Ricerca e sostituzione di testo"
simple_title:         "Ricerca e sostituzione di testo"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Semplice: spesso, quando si scrivono programmi, si vuole avere il controllo sui dati che vengono manipolati. La ricerca e la sostituzione di testo è uno strumento utile che permette di trovare e rimpiazzare parti specifiche di una stringa.

## Come fare

Per eseguire una ricerca e sostituzione di testo in Javascript, è possibile utilizzare il metodo `.replace()` delle stringhe. Il formato del metodo è il seguente: `stringa.replace( testoDaCercare, testoDaSostituire )`. Se il testo da cercare viene trovato, verrà sostituito con il testo da rimpiazzare. Un esempio concreto potrebbe essere:

```Javascript 
let testo = "Questa è una stringa di esempio";
let nuovoTesto = testo.replace("esempio", "prova");

console.log(nuovoTesto);
// Output: "Questa è una stringa di prova"
```

In questo caso, la parola "esempio" è stata trovata all'interno della stringa e sostituita con la parola "prova". Il metodo `.replace()` sostituisce solo la prima occorrenza trovata, quindi se si vuole sostituire tutte le occorrenze è necessario aggiungere il flag `"g"`, che sta per "global". Ad esempio:

```Javascript
let testo = "Questa è una stringa di esempio con diversi esempi";
let nuovoTesto = testo.replace(/esempio/g, "prova");

console.log(nuovoTesto);
// Output: "Questa è una stringa di prova con diversi prove"
```

In questo caso, il flag `g` ha permesso di sostituire tutte le occorrenze di "esempio" con "prova". È anche possibile utilizzare espressioni regolari all'interno del metodo `.replace()`, per avere una maggiore flessibilità e precisione nella ricerca del testo.

## Approfondimento

Il metodo `.replace()` in realtà è molto più potente e versatile di quanto si sia visto finora. Infatti, oltre al testo da rimpiazzare, è possibile passare anche una funzione come parametro di sostituzione. Questa funzione viene eseguita ogni volta che viene trovato il testo da cercare e può avere un input dinamico che dipende dalla posizione del testo cercato. Per esempio:

```Javascript
let testo = "Non ho mai avuto 23 anni, neanche ieri";
let nuovoTesto = testo.replace(/\d+/, function(numero) {
    return parseInt(numero) * 2;
});

console.log(nuovoTesto);
// Output: "Non ho mai avuto 46 anni, neanche ieri"
```

In questo caso, la funzione ha ricevuto come input il numero "23", lo ha moltiplicato per 2 e ha restituito il nuovo valore, che è stato poi utilizzato per sostituire il testo originale. Questo approccio può essere molto utile per manipolare dinamicamente i dati che si vogliono sostituire.

## Vedi anche

- [Documentazione ufficiale del metodo .replace()](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Tutorial su espressioni regolari in Javascript](https://www.w3schools.com/js/js_regexp.asp)
- [Articolo su come manipolare stringhe in Javascript](https://www.javascripttutorial.net/javascript-string/)