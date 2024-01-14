---
title:    "TypeScript: Conversi"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Perché

Convertire una stringa in minuscolo è una delle operazioni più comuni quando si lavora con stringhe in TypeScript. Ciò permette di uniformare il testo e facilitare la ricerca e il confronto tra le diverse stringhe. Vediamo come effettuare questa operazione.

## Come Fare

Per convertire una stringa in minuscolo in TypeScript, è possibile utilizzare il metodo `toLowerCase()` su un'istanza della stringa. Ad esempio:

```TypeScript
let stringa = "CIAO A TUTTI";
let stringaMinuscola = stringa.toLowerCase();

console.log(stringa); // Output: CIAO A TUTTI
console.log(stringaMinuscola); // Output: ciao a tutti
```

Come si può vedere dall'esempio sopra, il metodo `toLowerCase()` restituisce una nuova stringa convertita in minuscolo, lasciando invariata la stringa originale.

È importante notare che il metodo `toLowerCase()` utilizza la convenzione Unicode per convertire i caratteri in minuscolo. Ciò significa che alcuni caratteri speciali possono essere convertiti in modo diverso rispetto a quanto ci si aspetterebbe. Ad esempio, il carattere "I" maiuscolo con un accento acuto ("Í") viene convertito in "í" minuscolo. Se si desidera una conversione più accurata, è possibile utilizzare il metodo `toLocaleLowerCase()` che tiene conto delle impostazioni locali del sistema.

```TypeScript
let stringa = "COSE JUICY";
let stringaMinuscola = stringa.toLocaleLowerCase();

console.log(stringa); // Output: COSE JUICY
console.log(stringaMinuscola); // Output: cose juicy
```

## Deep Dive

Per comprendere meglio come funziona la conversione di una stringa in minuscolo, è importante capire il concetto di codifica dei caratteri. In informatica, la codifica dei caratteri è un sistema utilizzato per rappresentare caratteri di diversi alfabeti o simboli in modo da poter essere visualizzati e manipolati da un computer. Un esempio di codifica dei caratteri è Unicode, che è una convenzione accettata a livello internazionale che assegna un numero univoco a ogni carattere.

Nella codifica Unicode, ogni carattere viene rappresentato da un numero di codice che viene convertito in un punto di codice. Questo punto di codice è quindi utilizzato per determinare la forma finale del carattere visualizzato. Nel nostro caso, quando utilizziamo il metodo `toLowerCase()` su una stringa, viene utilizzato il punto di codice del carattere per determinare l'equivalente in minuscolo.

## See Also

- [Documentazione sul metodo toLocaleLowerCase() di TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-3-0.html#codepoint-at-method-ref):
Manuale ufficiale di TypeScript che descrive in dettaglio il metodo `toLocaleLowerCase()`.
- [Unicode Consortium](https://unicode.org/):
Sito ufficiale del consorzio Unicode con informazioni sui sistemi di codifica dei caratteri.