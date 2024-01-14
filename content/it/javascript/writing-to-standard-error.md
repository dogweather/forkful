---
title:                "Javascript: Scrivere su errore standard"
simple_title:         "Scrivere su errore standard"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error è un'importante abilità per i programmatori che vogliono diagnosticare e risolvere problemi nel loro codice. Questo processo permette di identificare gli errori e testare il funzionamento del programma in modo più dettagliato.

## Come fare

Per scrivere su standard error in Javascript, è necessario utilizzare il metodo `console.error()` e passare come parametro l'oggetto che si desidera visualizzare. Ad esempio: 

```Javascript 
let nome = "Mario"
console.error("Il nome è: " + nome); 
```

In questo caso, quando il programma viene eseguito, verrà mostrato l'errore "Il nome è: Mario" nella console degli errori.

## Approfondimento

Scrivere su standard error è utile perché consente di distinguere tra i messaggi di errore e i messaggi di output normali nella console del browser. Inoltre, è possibile specificare uno o più oggetti come parametri per fornire informazioni più dettagliate sull'errore. Ciò è particolarmente utile durante il debugging del codice.

## Vedi anche

Ecco alcuni link utili per saperne di più su come scrivere su standard error in Javascript:

- [Documentazione ufficiale di console.error()](https://developer.mozilla.org/it/docs/Web/API/Console/error)
- [Errore standard vs output standard](https://stackoverflow.com/questions/26349440/difference-between-standard-error-and-standard-output)
- [Come utilizzare la console degli errori in Chrome](https://developers.google.com/web/tools/chrome-devtools/console/errors)