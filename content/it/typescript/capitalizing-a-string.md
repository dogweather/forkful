---
title:                "Maiuscolizzare una stringa"
date:                  2024-01-19
simple_title:         "Maiuscolizzare una stringa"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Che Cos'è & Perché?
Capitalizzare una stringa significa trasformare la prima lettera di ogni parola in maiuscolo. I programmatori lo fanno per uniformare i formati dei testi, ad esempio nei titoli o nei nomi.

## Come Fare:
```TypeScript
function capitalizeString(input: string): string {
  return input.replace(/\b\w/g, firstChar => firstChar.toUpperCase());
}

const title = 'buongiorno a tutti, benvenuti alla programmazione TypeScript!';
const capitalizedTitle = capitalizeString(title);

console.log(capitalizedTitle);
// Output: 'Buongiorno A Tutti, Benvenuti Alla Programmazione TypeScript!'
```

## Approfondimento
Nel mondo della programmazione, la necessità di capitalizzare le stringhe risale ai primi giorni dell'elaborazione dei dati testuali. Nei contesti UI/UX, ad esempio, migliorare la leggibilità di titoli o intestazioni è essenziale.

Puoi anche trasformare in maiuscolo solo la prima lettera della stringa, o usare metodi di librerie esterne come Lodash (`_.capitalize`). Sotto al cofano, queste funzioni lavorano su ogni singolo carattere, confrontando e sostituendo le lettere dove necessario.

TypeScript non fornisce una funzione built-in per capitalizzare, quindi spesso si ricorre a funzioni personalizzate come sopra o si utilizzano estensioni di terze parti. Altre alternative includono l'uso di CSS per trasformazioni puramente visive (`text-transform: capitalize;`), ma questo non cambia il valore della stringa nel codice.

## Vedi Anche
- [MDN - String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [TypeScript Documentation](https://www.typescriptlang.org/docs/)
- [Lodash - capitalize method](https://lodash.com/docs/4.17.15#capitalize)
- [CSS Text Capitalization](https://developer.mozilla.org/en-US/docs/Web/CSS/text-transform)
