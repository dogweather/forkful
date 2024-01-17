---
title:                "Stampa dell'output di debug"
html_title:           "Javascript: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Stampare output di debug è un'azione comune tra i programmatori quando stanno risolvendo problemi o cercando di capire il funzionamento di una parte del codice. Questo tipo di output include informazioni utili come il valore delle variabili, i passaggi del loop e i messaggi di errore. Ciò aiuta i programmatori a individuare e risolvere gli errori più facilmente.

## Come fare:

```
const num = 20;
console.log(num); // output: 20

let fruits = ["apple", "banana", "orange"];
for (let i = 0; i < fruits.length; i++) {
  console.log(fruits[i]); // output: apple, banana, orange
}

if (num > 30) {
  console.log("the number is greater than 30"); // output: none
} else {
  console.log("the number is less than or equal to 30"); // output: the number is less than or equal to 30
}
```

## Approfondimento:

Stampare output di debug è una pratica comune nella programmazione fin dai primi giorni, quando i programmatori utilizzavano stampanti per visualizzare i messaggi di output. Oggi, questa pratica è diventata ancora più semplice grazie alla creazione di strumenti specifici come i debugger, che permettono ai programmatori di controllare il valore delle variabili e la traccia dell'esecuzione del codice.

Esistono anche alternative alla stampa di output di debug, come ad esempio l'utilizzo di un logger per registrare i messaggi di errore e di debug su file di log. Questo può essere utile soprattutto in applicazioni più complesse.

Per implementare la stampa di output di debug, si utilizza il metodo console.log() che accetta uno o più argomenti e li stampa nella console del browser o della console del terminale. Inoltre, è possibile utilizzare variabili di ambiente per abilitare o disabilitare la stampa di output di debug a seconda dell'ambiente in cui viene eseguito il codice.

## Vedi anche:

- [Console.log() - MDN Web Docs](https://developer.mozilla.org/it/docs/Web/API/Console/log)
- [Debugging con il metodo console.log() - W3Schools](https://www.w3schools.com/js/js_debugging.asp)
- [Logging with Console.log() - javascript.info](https://javascript.info/debugging-chrome)