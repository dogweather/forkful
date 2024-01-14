---
title:    "TypeScript: Estrazione di sottostringhe"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Perché

L'estrazione di sottostringhe è un'operazione comune nella programmazione. Ciò consente di ottenere solo una parte di una stringa più grande, il che può essere utile per manipolarla o visualizzarla in modo diverso. Vediamo come fare per ottenere sottostringhe utilizzando TypeScript.

## Come fare

Estrarre substrings è abbastanza semplice utilizzando i metodi built-in delle stringhe in TypeScript. Per ottenere una sottostringa da una stringa, possiamo utilizzare il metodo `substring (startIndex, endIndex)`.

```TypeScript
const frase = "Ciao a tutti";
const sottostringa = frase.substring(0, 4);

console.log(sottostringa); // Output: Ciao
```

In questo esempio, stiamo estraendo la sottostringa dalla posizione 0 alla posizione 3 di `frase`, che equivale alle prime 4 lettere. Il parametro `endIndex` non viene incluso nella sottostringa.

## Approfondimento

Oltre al metodo `substring`, ci sono altri modi per estrarre sottostringhe in TypeScript. Ad esempio, possiamo utilizzare il metodo `slice` o usare l'operatore di accesso `[]` sulla stringa. Inoltre, TypeScript ci consente di utilizzare la sintassi dei punti e delle parentesi per accedere alle proprietà di una stringa.

```TypeScript
const frase = "Ciao a tutti";
const sottostringa1 = frase.slice(5, 8);
const sottostringa2 = frase[0];
const sottostringa3 = frase.charAt(0);

console.log(sottostringa1); // Output: tutti
console.log(sottostringa2); // Output: C
console.log(sottostringa3); // Output: C
```

In questo esempio, utilizziamo il metodo `slice` per estrarre la sottostringa dalla posizione 5 alla posizione 7 di `frase`, che corrisponde alle ultime tre lettere. Utilizzando l'operatore di accesso `[]` e il metodo `charAt`, possiamo estrarre la prima lettera di `frase`.

## Vedi anche

- [Documentazione stringhe TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html)
- [Metodi delle stringhe in TypeScript](https://www.tutorialspoint.com/typescript/typescript_strings.htm)
- [Estrazione di sottostringhe in JavaScript](https://www.w3schools.com/jsref/jsref_substring.asp)