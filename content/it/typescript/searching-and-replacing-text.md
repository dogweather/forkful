---
title:    "TypeScript: Cerca e sostituisci testo"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Perché

Il processo di ricerca e sostituzione di testo è una delle attività più comuni quando si lavora con codice TypeScript. È utile per correggere errori di battitura, aggiornare nomi di variabili e molto altro ancora.

## Come Fare

Per eseguire una ricerca e sostituzione di testo in TypeScript, possiamo utilizzare il metodo `replace()` che accetta come argomenti il valore da cercare e il nuovo valore da sostituire.

```TypeScript
const frase = "Ciao, come stai?";
const nuovaFrase = frase.replace("stai", "va");
console.log(nuovaFrase);
```

Output: `Ciao, come va?`

Possiamo anche utilizzare un'espressione regolare per eseguire una ricerca e sostituzione più avanzata. Ad esempio, se volessimo sostituire tutte le vocali di una stringa con un asterisco, possiamo fare così:

```TypeScript
const parola = "javascript";
const nuovaParola = parola.replace(/[aeiou]/g, "*");
console.log(nuovaParola);
```

Output: `j*v*scr*pt`

## Deep Dive

Il metodo `replace()` accetta anche una funzione come secondo argomento, che ci permette di eseguire una sostituzione personalizzata. La funzione riceve come argomenti il valore trovato, l'indice in cui si trova nella stringa originale e la stringa originale stessa. Ad esempio:

```TypeScript
const frutta = "mela, banana, pera";
const nuovaFrutta = frutta.replace(/\w+/g, (match, index) => match + index);
console.log(nuovaFrutta);
```

Output: `mela0, banana6, pera12`

In questo esempio, la funzione aggiunge l'indice in cui viene trovato ogni valore alla fine di esso.

## Vedi Anche

- [Documentazione sul metodo replace di TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html#string-replace)
- [RegExr - Strumento online per testare e imparare le espressioni regolari](https://regexr.com/)
- [Articolo sulla gestione delle stringhe in TypeScript](https://blog.bitsrc.io/string-manipulation-in-typescript-471b0a3a87f2)