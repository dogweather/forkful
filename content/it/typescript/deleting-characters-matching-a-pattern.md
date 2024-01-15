---
title:                "Eliminazione dei caratteri corrispondenti a un modello"
html_title:           "TypeScript: Eliminazione dei caratteri corrispondenti a un modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un modello"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni che possono spingere qualcuno a voler cancellare dei caratteri che corrispondono ad un determinato pattern. Potrebbe essere necessario pulire una stringa di dati prima di elaborarla, oppure semplicemente rimuovere delle parti indesiderate da un testo.

## Come Fare

Per cancellare dei caratteri che corrispondono ad un certo pattern in TypeScript, possiamo utilizzare il metodo nativo `replace()` della classe `String`. In questo metodo, possiamo fornire come parametro una regex (espressione regolare) che identifichi i caratteri che vogliamo eliminare e un valore di sostituzione. Ad esempio, per eliminare tutti i numeri da una stringa possiamo utilizzare il seguente codice:

```TypeScript
const stringa = "123 Lorem ipsum 456 dolor sit 789";
const nuovaStringa = stringa.replace(/[0-9]/g, ""); // output: " Lorem ipsum  dolor sit "
```

In questo esempio, la regex `/[0-9]/g` corrisponde a qualsiasi numero presente nella stringa e il secondo parametro della funzione `replace` è una stringa vuota, quindi quei caratteri verranno sostituiti con il nulla.

## Approfondimento

In TypeScript, le regex (espressioni regolari) sono una potente strumento per manipolare le stringhe. Possiamo utilizzarle non solo per eliminare caratteri, ma anche per trovare e sostituire porzioni di testo, validare input utente e molto altro ancora. Esistono diverse funzioni e operatori per gestire le regex in TypeScript, quindi consiglio di approfondire l'argomento per scoprire tutte le loro potenzialità.

## Vedi Anche

- [Documentazione ufficiale di TypeScript sulle regex](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Tutorial su come utilizzare le espressioni regolari in TypeScript](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-typescript)
- [Un'ulteriore introduzione alle regex in TypeScript](https://codeburst.io/regular-expressions-in-typescript-explanation-with-examples-214e45522614)