---
title:                "Ottenere la data corrente"
html_title:           "Java: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Ottenere la data corrente significa ritirare le informazioni sulla data e l'ora esatte nel momento in cui un pezzo di codice viene eseguito. I programmatori lo fanno per vari motivi, come timestampare gli eventi, programmare gli avvisi o calcolare il tempo trascorso.

## Come si fa:
Per ottenere la data corrente in TypeScript, usiamo l'oggetto Date incorporato e il suo metodo `toISOString()`. Ecco un esempio:

```TypeScript
let now = new Date();
console.log(now.toISOString()); 
```

Se esegui questo frammento di codice, otterrai un risultato simile a:

```TypeScript 
'2022-01-01T00:00:00.000Z' 
```

## Approfondimenti
L'oggetto Date in JavaScript (e quindi in TypeScript) esiste fin dalla creazione di JavaScript. Ha subito alcune modifiche nel corso degli anni, ma l'essenza è rimasta invariata. 

Ci sono metodi alternativi per ottenere la data corrente, ad esempio, potrebbe essere più appropriato per alcuni casi d'uso usare `Date.now()` che restituisce il timestamp Unix, ovvero il numero di millisecondi trascorsi dal 1 gennaio 1970.

```TypeScript
let timestamp = Date.now();
console.log(timestamp);
```

Otterrai un risultato come questo:

```TypeScript
1642430253421
```

Riguardo i dettagli di implementazione, intrinsecamente, TypeScript si basa su JavaScript. Quindi, quando usi l'oggetto Date in TypeScript, stai in realtà usando l'oggetto Date JavaScript. TypeScript esiste per fornire tipi statici e altre funzionalità utili, ma non modifica il comportamento di base di JavaScript.

## Vedi Anche
Se vuoi approfondire l'argomento, qui ci sono alcuni link utili:

- Documentazione MDN su `Date()`: https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date
- Guida TypeScript per principianti di freeCodeCamp: https://www.freecodecamp.org/news/the-typescript-guide-for-javascript-developers/
- La guida pratica di basarat su TypeScript: https://basarat.gitbook.io/typescript/
  
Ricorda, la pratica è la chiave per diventare un esperto! Buona codifica!