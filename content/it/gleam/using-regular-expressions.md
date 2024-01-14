---
title:                "Gleam: Utilizzo delle espressioni regolari"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché

Le espressioni regolari sono una potente funzionalità di programmazione che consentono di cercare e manipolare testo in modo efficiente. Sono ampiamente utilizzate per la validazione di input, la ricerca di pattern specifici e il filtraggio dei dati. Se sei uno sviluppatore che lavora con testo, le espressioni regolari possono semplificare e velocizzare il tuo lavoro.

## Come Utilizzarle

Per utilizzare le espressioni regolari in Gleam, è necessario importare il modulo `regex` utilizzando l'istruzione `import` seguita dal nome del modulo. Quindi puoi utilizzare diverse funzioni del modulo per creare ed eseguire le tue espressioni regolari.

Ecco un semplice esempio di come trovare e stampare un pattern all'interno di una stringa utilizzando le espressioni regolari:

```Gleam
import regex

let pattern = regex.compile(`world`, "") // crea un'espressione regolare che cerca la parola "world"
let match = pattern.find("Hello world!") // cerca il pattern nella stringa
match.map(text => println(text)) // stampa il testo trovato, se presente
```

L'output di questo codice sarà `world`, poiché è la parola che corrisponde al nostro pattern.

È possibile utilizzare diverse opzioni per modificare il comportamento delle espressioni regolari, come ad esempio l'utilizzo di espressioni case-insensitive o l'uso di quantificatori per trovare più occorrenze di un pattern. Il modulo `regex` offre molte funzioni utili per gestire queste opzioni e creare espressioni regolari più sofisticate.

## Approfondimenti

Per saperne di più sulle espressioni regolari in Gleam, è possibile consultare la documentazione ufficiale del modulo `regex` che include esempi di codice e spiegazioni dettagliate. Inoltre, è consigliabile esplorare e praticare con le espressioni regolari utilizzando quiz e tutorial online per migliorare la propria comprensione e padronanza di questa funzionalità.

## Vedi Anche

- [Documentazione ufficiale del modulo `regex`](https://gleam.run/modules/regex)
- [Esercizi e tutorial per espressioni regolari](https://regexone.com/)
- [Quiz interattivo su espressioni regolari](https://regexcrossword.com/)