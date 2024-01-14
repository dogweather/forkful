---
title:    "Javascript: Stampa dell'output di debug"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

##Perché

Debuggare il proprio codice è un'attività fondamentale per garantire il corretto funzionamento di un programma. La stampa degli output di debug è uno dei metodi più utilizzati per identificare errori e trovare soluzioni.

##Come fare

Per stampare un output di debug in Javascript, è possibile utilizzare la funzione `console.log()`. Questa funzione accetta come parametro una variabile o una stringa e la stampa in console durante l'esecuzione del codice.

Esempio:

```Javascript
var numero = 10;
console.log("Il numero è: " + numero);
```

Questo codice stampa in console la stringa "Il numero è: 10". Possiamo anche stampare più variabili o stringhe concatenandole all'interno della funzione `console.log()`.

Esempio:

```Javascript
var testo = "Questo è un esempio";
var numero = 20;
console.log("Test: " + testo + " - Numero: " + numero);
```

Output:

```
Test: Questo è un esempio - Numero: 20
```

##Approfondimento

La stampa degli output di debug non deve essere utilizzata esclusivamente per rilevare errori, ma può essere anche uno strumento utile per monitorare l'esecuzione del codice. È possibile inserire più istruzioni `console.log()` all'interno di una funzione per capire il flusso di esecuzione e verificare il valore delle variabili in diversi momenti.

Inoltre, è possibile utilizzare il metodo `.toString()` per convertire una variabile di tipo oggetto in una stringa e stamparla come output di debug.

Esempio:

```Javascript
var persona = {
  nome: "Paolo",
  cognome: "Rossi",
  eta: 30
};

console.log("Persona: " + persona.toString());
```

Output:

```
Persona: [object Object]
```

###Vedi anche

- Documentazione ufficiale di `console.log()`: https://developer.mozilla.org/it/docs/Web/API/Console/log
- Articolo su come utilizzare la stampa di debug per debuggare il proprio codice: https://www.html.it/articoli/metodi-di-debugging-in-javascript/