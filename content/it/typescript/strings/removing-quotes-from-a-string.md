---
title:                "Rimuovere le virgolette da una stringa"
aliases: - /it/typescript/removing-quotes-from-a-string.md
date:                  2024-01-26T03:42:25.830087-07:00
model:                 gpt-4-0125-preview
simple_title:         "Rimuovere le virgolette da una stringa"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Che cosa e perché?
Rimuovere le virgolette da una stringa significa togliere i caratteri di virgoletta singola (`'`) o doppia (`"`) che definiscono le stringhe letterali nel codice. I programmatori lo fanno per diverse ragioni, come formattare l'output, sanificare l'input dell'utente o preparare le stringhe per l'analisi o l'archiviazione dove le virgolette sono inutili o potrebbero causare errori.

## Come fare:
Ecco una guida diretta per liberare le tue stringhe da quei fastidiosi segni di virgoletta in TypeScript.

```typescript
// Opzione A: Sostituire le virgolette singole o doppie usando regex
function removeQuotes(input: string): string {
  return input.replace(/^["']|["']$/g, '');
}

console.log(removeQuotes(`"Stringa quotata"`)); // Stringa quotata
console.log(removeQuotes(`'Un'altra ancora'`)); // Un'altra ancora

// Opzione B: Gestire le stringhe che iniziano e finiscono con virgolette diverse
function removeMismatchedQuotes(input: string): string {
  return input.replace(/^(['"])(.*?)(?<!\1)\1$/, '$2');
}

console.log(removeMismatchedQuotes(`"Virgolette disallineate'`)); // "Virgolette disallineate'

// Opzione C: Rimuovere più tipi di virgolette
function removeAllQuotes(input: string): string {
  return input.replace(/['"]+/g, '');
}

console.log(removeAllQuotes(`"'Mix'n'Match'"`)); // Mix'n'Match
```

## Approfondimento
Ben prima che TypeScript fosse anche solo una cosa, i coder JavaScript già affrontavano le questioni delle virgolette, e la storia è più o meno la stessa per TypeScript. Col passare del tempo, cambia anche il modo in cui segmentiamo le stringhe. Oggi giorno, con la forza muscolare delle regex, mettiamo da parte il taglio goffo delle stringhe o altri metodi tediosi.

Sebbene gli esempi sopra dovrebbero coprire la maggior parte delle tue necessità, ricorda, l'uso delle virgolette può diventare complesso. Virgolette nidificate, disallineate ed escape sono i trucchetti pronti a farti inciampare. Per questi, potresti aver bisogno di schemi più sofisticati o addirittura di parser per gestire ogni caso ricciolo.

Alternative? Alcune persone preferiscono affidarsi a librerie come lodash, con metodi come `trim` e `trimStart` / `trimEnd`, che possono essere adattati per tagliare le virgolette se imposti i caratteri che vuoi rimuovere.

E per voi appassionati di TypeScript, non dimentichiamoci dei tipi. Sebbene qui ci occupiamo principalmente di stringhe, quando lavorate con l'input dell'utente o con l'analisi, introdurre alcuni controlli di tipo o addirittura generici può aiutare a garantire che il vostro codice sia sicuro tanto quanto le vostre virgolette sono tagliate.

## Vedi Anche
Visita questi hotspot virtuali per maggiori informazioni:

- MDN Web Docs su regex (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- Documentazione ufficiale di TypeScript (https://www.typescriptlang.org/docs/)
- You Don't Need Lodash/Underscore – Aiuti per le Stringhe (https://github.com/you-dont-need/You-Dont-Need-Lodash-Underscore#strings)
- Stack Overflow: Attraversa le trincee dove innumerevoli sviluppatori hanno combattuto catastrofi delle virgolette (https://stackoverflow.com/)
