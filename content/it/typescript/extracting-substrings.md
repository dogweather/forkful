---
title:                "Estrazione di sottostringhe"
html_title:           "TypeScript: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché estrarre sottostringhe?

Se hai familiarità con la programmazione, probabilmente hai incontrato situazioni in cui era necessario estrarre una parte di una stringa più lunga. Ciò può essere dovuto alla necessità di manipolare o analizzare i dati in modo più efficiente. In TypeScript, ci sono diverse opzioni per estrarre sottostringhe e in questo articolo esploreremo come farlo in modo semplice ed efficace.

## Come fare

Estrarre sottostringhe in TypeScript è un processo relativamente semplice e può essere fatto utilizzando il metodo `.slice()` oppure tramite l'utilizzo delle espressioni regolari. Vediamo alcuni esempi pratici di entrambi i metodi utilizzando la seguente stringa di esempio:

```TypeScript
const stringa = "Questo è un esempio di stringa";
```

### Metodo slice()
Il metodo `.slice()` permette di estrarre una sezione specifica di una stringa sulla base degli indici di inizio e fine. Questi indici devono essere passati come argomenti nel seguente modo: `.slice(indiceInizio, indiceFine)`. Vale la pena notare che il carattere corrispondente all'indice finale non viene incluso nell'estrazione della sottostringa. Vediamo un esempio:

```TypeScript
console.log(stringa.slice(6,8)); // output: è
```

### Espressioni regolari
Un'altra opzione per estrarre sottostringhe in TypeScript è l'utilizzo delle espressioni regolari. Questo metodo offre una maggiore flessibilità in termini di criteri di ricerca. Vediamo un esempio di come utilizzare l'espressione regolare per estrarre la parola "esempio" dalla nostra stringa di esempio:

```TypeScript
const esempio = stringa.match(/esempio/g); 
console.log(esempio); // output: [ 'esempio' ]
```

Come puoi vedere, in questo caso è stata creata una variabile `esempio` che contiene l'array delle corrispondenze trovate tramite l'espressione regolare. Se volessimo estrarre una sottostringa specifica da questa corrispondenza, possiamo utilizzare il metodo `.slice()` come nell'esempio precedente.

## Deep Dive

Per coloro che sono interessati a conoscere più approfonditamente il funzionamento delle espressioni regolari, TypeScript offre la possibilità di utilizzare l'operatore `[]` per specificare un gruppo di caratteri da cercare. Ad esempio, possiamo creare un'espressione regolare per estrarre tutte le vocali dalla nostra stringa di esempio:

```TypeScript
const vocali = stringa.match(/[aeiou]/g); 
console.log(vocali); // output: ['i', 'a', 'i', 'e', 'e', 'o', 'a', 'i', 'a', 'a']
```

Come puoi vedere, l'operatore `[]` consente di specificare un gruppo di caratteri o intervalli di caratteri da cercare all'interno della stringa. Se vuoi saperne di più sulle espressioni regolari, ti consiglio di consultare la documentazione ufficiale di TypeScript.

## See Also

- Documentazione ufficiale di TypeScript su espressioni regolari: https://www.typescriptlang.org/docs/handbook/declarations-regex.html
- Altro articolo su come estrarre sottostringhe in TypeScript: https://www.tutorialsteacher.com/typescript/typescript-string
- Articolo su come utilizzare il metodo `.slice()` in TypeScript: https://www.javatpoint.com/typescript-string-slice