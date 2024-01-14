---
title:    "TypeScript: Convertire una data in una stringa"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Perché convertire una data in una stringa

Spesso, quando lavoriamo con date in TypeScript, ci troviamo nella situazione di dover convertire una data in una stringa. Questo può essere necessario per stampare una data in un formato specifico o per passarla come parametro a una funzione.

## Come farlo

Per convertire una data in una stringa in TypeScript, possiamo utilizzare il metodo `toLocaleString()`. Prendiamo ad esempio una data rappresentata dall'oggetto `Date`. Basta chiamare il metodo `toLocaleString()` su questo oggetto per ottenere una stringa nel formato locale corrente. Vediamo un esempio pratico:

```TypeScript
let data = new Date();
console.log(data.toLocaleString()); // Output: 13/05/2021, 11:25:32
```

Possiamo anche specificare il formato della stringa che vogliamo ottenere utilizzando il metodo `toLocaleDateString()` o `toLocaleTimeString()` a seconda delle nostre esigenze.

```TypeScript
let data = new Date();
console.log(data.toLocaleDateString()); // Output: 13/05/2021
console.log(data.toLocaleTimeString()); // Output: 11:25:32
```

Inoltre, possiamo passare alcune opzioni come parametro al metodo `toLocaleString()` per configurare il formato della stringa in base alle nostre preferenze. Ad esempio, possiamo specificare il linguaggio o il fuso orario.

```TypeScript
let data = new Date();
let options = { weekday: 'long', month: 'long', day: 'numeric', year: 'numeric' };
console.log(data.toLocaleString('en-US', options)); // Output: Thursday, May 13, 2021
```

## Approfondimento

La conversione di una data in una stringa può sembrare un'operazione semplice, ma ha in realtà alcune sfumature che è importante conoscere. Ad esempio, il metodo `toLocaleString()` utilizza il formato della data e dell'ora specifici della lingua e del locale del browser. Se vogliamo ottenere un formato diverso, dobbiamo passare delle opzioni come parametro come visto nell'esempio precedente.

Inoltre, è importante considerare il formato della data utilizzato nel nostro codice e quello di default del browser. Ad esempio, se il nostro codice utilizza il formato "dd/MM/yyyy", ma il browser è impostato su "MM/dd/yyyy", otterremo risultati diversi dalla conversione della data in stringa.

Un'altra cosa da tenere a mente è che il metodo `toLocaleString()` restituisce una stringa locale, quindi se il nostro codice viene eseguito in un altro paese, la data verrà rappresentata nel formato e nella lingua del paese in cui viene eseguito.

## Vedi anche

- [MDN - Date.prototype.toLocaleString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString)
- [MDN - Options parameter for toLocaleString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString#parameters)