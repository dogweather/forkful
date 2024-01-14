---
title:                "TypeScript: Estrazione di sottostringhe"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

L'estrazione di sottostringhe è una pratica comune nella programmazione TypeScript utilizzata per ottenere una parte specifica di una stringa più lunga. Questo può essere utile per manipolare e ottenere informazioni specifiche da una stringa, come ad esempio i caratteri in una posizione particolare o un numero di caratteri preciso. In questo articolo, esploreremo come e perché utilizzare la funzione di estrazione delle sottostringhe in TypeScript.

## Come Fare

Per estrarre una sottostringa da una stringa più grande, possiamo utilizzare il metodo `substring()` di TypeScript. Ecco un semplice esempio:

```TypeScript
let str: string = "Ciao mondo!";
let substr: string = str.substring(0, 4);
console.log(substr);
```

In questo esempio, abbiamo una stringa di testo "Ciao mondo!" e utilizziamo il metodo `substring()` per estrarre i primi 4 caratteri, ovvero "Ciao". Questo perché il parametro `0` indica da quale posizione iniziare l'estrazione, mentre il parametro `4` indica quanti caratteri vogliamo estrarre.

Il metodo `substring()` è molto flessibile e offre diverse opzioni per l'estrazione di sottostringhe. Possiamo utilizzare anche due parametri, dove il primo indica la posizione di inizio e il secondo la posizione finale per estrarre la sottostringa. Vediamo un altro esempio:

```TypeScript
let str: string = "Ciao amici!";
let substr: string = str.substring(5, 11);
console.log(substr);
```

In questo caso, stiamo estraendo la sottostringa "amici" a partire dalla posizione 5 (inclusa) fino alla posizione finale 11 (esclusa).

Possiamo anche utilizzare il metodo `substring()` per estrarre una sottostringa a partire dalla fine di una stringa utilizzando numeri negativi. Ad esempio:

```TypeScript
let str: string = "Buongiorno";
let substr: string = str.substring(-5);
console.log(substr);
```

In questo esempio, il parametro `-5` indica che vogliamo estrarre una sottostringa di lunghezza 5 a partire dalla fine della stringa, ottenendo come risultato "giorno".

## Approfondimento

Oltre al metodo `substring()`, TypeScript offre anche altre funzioni per l'estrazione di sottostringhe, come `slice()`, `substr()` e `substring()`. Ognuna di queste funzioni ha delle differenze nella loro implementazione ed è importante comprendere le loro peculiarità per scegliere quella più adatta al nostro scopo. Inoltre, è sempre consigliabile effettuare controlli sulle posizioni e sulle lunghezze delle stringhe per evitare errori di compilazione.

## Vedi Anche

- [Documentazione ufficiale di TypeScript sulle sottostringhe](https://www.typescriptlang.org/docs/handbook/strings.html#substring-and-substr)
- [Esempi di estrazione di sottostringhe in TypeScript](https://www.tutorialspoint.com/typescript/typescript_strings.htm)