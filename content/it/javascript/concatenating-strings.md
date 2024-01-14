---
title:                "Javascript: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

La concatenazione di stringhe è un'operazione molto comune nei linguaggi di programmazione, in particolare in Javascript. Ciò significa unire due o più stringhe per crearne una nuova. Questo può essere utile in molteplici casi, ad esempio per creare output personalizzati o per manipolare e organizzare i dati.

## Come

Per concatenare le stringhe in Javascript, si utilizza l'operatore più (+). Ecco un esempio di codice:

```Javascript
let nome = "Maria";
let cognome = "Rossi";
let nomeCompleto = nome + " " + cognome;
console.log(nomeCompleto);
```

L'output sarà "Maria Rossi". È importante notare che per separare le stringhe è necessario utilizzare un carattere vuoto (" ") o un carattere di punteggiatura, come una virgola.

## Deep Dive

Esistono diverse alternative per concatenare le stringhe in Javascript. Una di queste è utilizzare il metodo `concat()`, che permette di unire più stringhe nello stesso modo dell'operatore più, ma con la possibilità di aggiungere più stringhe in una sola volta. Ecco un esempio:

```Javascript
let nome = "Maria";
let cognome = "Rossi";
let età = 25;
let info = nome.concat(" ", cognome, " ha ", età, " anni");
console.log(info);
```

L'output sarà "Maria Rossi ha 25 anni". Inoltre, è possibile utilizzare il metodo `join()` per concatenare più stringhe mediante l'uso di un separatore specifico, come in questo esempio:

```Javascript
let listaSpesa = ["pane", "latte", "uova"];
let spesa = listaSpesa.join(", ");
console.log("La mia lista della spesa: " + spesa);
```

L'output sarà "La mia lista della spesa: pane, latte, uova".

## Vedi Anche

- [Documentazione ufficiale di Javascript sulla concatenazione di stringhe](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [Tutorial su come concatenare stringhe in Javascript](https://www.w3schools.com/js/js_string_concat.asp)
- [Altri metodi per manipolare le stringhe in Javascript](https://www.sitepoint.com/javascript-string-manipulation/)