---
title:                "TypeScript: Cancellazione di caratteri corrispondenti a un modello"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Spesso, durante lo sviluppo di un'applicazione, ci troviamo di fronte alla necessità di manipolare stringhe di testo. Una delle operazioni più comuni è la rimozione di caratteri che corrispondono ad un determinato pattern. In questo breve articolo parleremo di come affrontare questo problema utilizzando TypeScript.

## Come Effettuare la Rimozione di Caratteri Corrispondenti ad un Pattern Utilizzando TypeScript

Per cominciare, vediamo un esempio di codice che mostra come utilizzare il metodo `replace` per eliminare tutti i caratteri numerici da una stringa:

```TypeScript
let testString = "Questa è una stringa123 di testo456."
let pattern = /\d+/g; // pattern per trovare qualsiasi carattere numerico

let newString = testString.replace(pattern, ''); // sostituisce i caratteri numerici con una stringa vuota

console.log(newString); // Output: "Questa è una stringa di testo."
```

In questo esempio, utilizziamo una regex (espressione regolare) per definire il pattern, che verrà poi utilizzato come parametro del metodo `replace`. Il primo parametro del metodo è il pattern da cercare, mentre il secondo è la stringa che sostituirà il pattern trovato. In questo caso, abbiamo semplicemente sostituito il pattern con una stringa vuota.

È importante notare che il metodo `replace` non modifica la stringa originale, ma restituisce invece una nuova stringa con le modifiche applicate. Quindi, se desideriamo salvare la nuova stringa, dobbiamo assegnarla ad una variabile.

Un altro metodo utile è `split`, che divide una stringa in un array di sottostringhe. Possiamo utilizzare questo metodo per rimuovere i caratteri di punteggiatura da una stringa. Ad esempio:

```TypeScript
let testString = "Questa è una stringa di testo, con la quale voglio rimuovere i caratteri di puntazione!"
let pattern = /[~`!@#$%^&*(){}\[\];:"'<,.>?\/\\|_+=-]/g; // pattern per rimuovere i caratteri di punteggiatura

let newString = testString.split(pattern).join(''); // rimuove i caratteri di punteggiatura e unisce le sottostringhe ottenute
console.log(newString); // Output: "Questa è una stringa di testo con la quale voglio rimuovere i caratteri di puntazione."
```

In questo caso, utilizziamo `split` per dividere la stringa in un array di sottostringhe, utilizzando il nostro pattern come delimitatore. Poi, utilizziamo `join` per unire le sottostringhe ottenute in una nuova stringa senza i caratteri di punteggiatura.

## Approfondimento sulla Rimozione di Caratteri Corrispondenti ad un Pattern

Se desideriamo rimuovere specifici caratteri da una stringa, possiamo utilizzare il metodo `filter` per creare un nuovo array contenente solo i caratteri che desideriamo mantenere. Ad esempio, se vogliamo mantenere solo le vocali in una stringa, possiamo fare così:

```TypeScript
let testString = "Questa è una stringa di testo."
let vowelsPattern = /[aeiou]/g; // pattern per trovare le vocali
let consonantsPattern = /[bcdfghlmnpqrstvxywz]/g; // pattern per trovare le consonanti

let newString = testString.split('').filter(char => char.match(vowelsPattern)).join('');

console.log(newString); // Output: "uea"

let newString2 = testString.split('').filter(char => char.match(consonantsPattern)).join('');

console.log(newString2); // Output: "Qtt sn sgrp d tst."
```

In questo esempio, utilizziamo `split` per dividere la stringa in un array di caratteri, applichiamo il metodo `filter` sull'array utilizzando il nostro pattern per mantenere solo i caratteri che vogliamo mantenere, e poi utilizziamo `join` per creare una nuova stringa con i caratteri rimanenti.

## Vedi Anche

- [Documentazione di TypeScript: Metodi per la Gestione delle Stringhe](https://www.typescriptlang.org/docs/handbook/