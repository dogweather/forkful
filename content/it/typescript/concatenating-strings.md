---
title:    "TypeScript: Concatenazione di stringhe"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché
Lo scopo dell'operazione di concatenazione di stringhe in TypeScript è quello di unire due o più stringhe in una singola stringa. Ciò può essere utile per creare una stringa più lunga o per formattare una stringa in modo specifico.

## Come fare
Per concatenare stringhe in TypeScript, si può utilizzare l'operatore '+' tra le stringhe. Vediamo un esempio:

```TypeScript
let nome = "Carlo";
let cognome = "Rossi";
let nomeCompleto = nome + " " + cognome;
console.log(nomeCompleto);
```

Nell'esempio sopra, abbiamo creato tre variabili: "nome", "cognome" e "nomeCompleto". Utilizzando l'operatore '+', abbiamo unito le stringhe "Carlo" e "Rossi" nella variabile "nomeCompleto". Infine, utilizzando il metodo "console.log()", abbiamo stampato a schermo la stringa risultante: "Carlo Rossi".

Oltre all'operatore '+', si può anche utilizzare il metodo "concat()" per concatenare stringhe. Vediamo un esempio:

```TypeScript
let saluto = "Ciao";
let nome = "Maria";
let salutoCompleto = saluto.concat(" ", nome);
console.log(salutoCompleto);
```

In questo caso, abbiamo utilizzato il metodo "concat()" per unire la stringa "Ciao" con lo spazio vuoto e la stringa "Maria" nella variabile "salutoCompleto". Questo ci darà lo stesso risultato del primo esempio: "Ciao Maria".

## Approfondimento
In TypeScript, esistono diverse modalità per concatenare tipi di dati diversi, non solo stringhe. Ad esempio, si può concatenare una stringa con un numero:

```TypeScript
let fraseIniziale = "Il tuo punteggio è: ";
let punteggio = 75;
let fraseFinale = fraseIniziale + punteggio;
console.log(fraseFinale);
```

In questo caso, il risultato finale sarà "Il tuo punteggio è: 75", poiché il valore della variabile "punteggio" viene convertito in una stringa automaticamente durante l'operazione di concatenazione.

E' anche importante notare che l'operazione di concatenazione non modifica le variabili originali, ma crea una nuova stringa. Quindi, se vogliamo utilizzare la stringa concatenata in una nuova variabile, dobbiamo assegnarla ad essa.

## Vedi anche
- Documentazione ufficiale di TypeScript sull'operatore '+': https://www.typescriptlang.org/docs/handbook/2/objects.html#structuring-with-operators
- Tutorial su come concatenare stringhe in TypeScript: https://www.digitalocean.com/community/tutorials/how-to-concatenate-strings-in-typescript 
- Tutorial su come utilizzare il metodo "concat()": https://www.geeksforgeeks.org/typescript-concat-function/