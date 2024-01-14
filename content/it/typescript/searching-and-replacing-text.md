---
title:    "TypeScript: Ricerca e sostituzione di testo"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

C'è una cosa che tutti noi programmatori abbiamo in comune: dobbiamo lavorare con il testo. Potrebbe trattarsi di inserire del testo in un formulario, di stampare del testo a schermo o di manipolare il testo in modo dinamico. Ma a volte abbiamo bisogno di apportare delle modifiche al nostro testo. Forse vogliamo cambiare una parola o una frase in tutto il nostro codice, o forse vogliamo sostituire una variabile con un nuovo valore. In questi casi, l'utilizzo di una funzione di ricerca e sostituzione può essere un grande risparmio di tempo e fatica. Ecco perché imparare come farlo in TypeScript è una competenza importante per ogni programmatore.

## Come fare

Per eseguire una ricerca e sostituzione nel tuo codice TypeScript, puoi utilizzare il metodo `replace()` di una stringa. Prende due parametri: il testo da cercare e il testo con cui sostituirlo. Vediamo un esempio pratico:

```TypeScript
let testo: string = "Ciao a tutti!";
testo = testo.replace("Ciao", "Salve");
console.log(testo);
```

L'output di questo codice sarà: `Salve a tutti!`. Come puoi vedere, la parola "Ciao" è stata sostituita con "Salve". Ora, questo esempio è molto semplice, ma puoi applicare lo stesso concetto per sostituire qualsiasi testo all'interno di una stringa.

## Approfondimento

Se vuoi approfondire la tua conoscenza sulla ricerca e sostituzione in TypeScript, ci sono alcune opzioni che possono tornarti utili. Per esempio, puoi usare espressioni regolari per cercare e sostituire testo in modo più flessibile e dinamico. Inoltre, puoi combinare il metodo `replace()` con altri metodi come `split()` e `join()` per effettuare sostituzioni su un array di stringhe. Inoltre, potresti aver bisogno di gestire caratteri speciali nel tuo testo come gli apici. Per questo, puoi utilizzare il metodo `replaceAll()` invece di `replace()`, che sostituirà tutte le occorrenze della stringa cercata.

## Vedi anche

- [Documentazione ufficiale di TypeScript sul metodo replace()](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-1.html#template-literal-types)
- [Un tutorial su come utilizzare le espressioni regolari in TypeScript](https://www.digitalocean.com/community/tutorials/js-regular-expressions-in-typescript)
- [Una guida su come gestire gli apici nel codice TypeScript](https://codeburst.io/typescript-escaping-special-characters-4e95b65f98b?gi=c572cbd85e06)