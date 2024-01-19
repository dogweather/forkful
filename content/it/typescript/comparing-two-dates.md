---
title:                "Confronto tra due date"
html_title:           "Elixir: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Comparare due date significa determinare se una data è precedente, successiva o uguale ad un'altra. I programmatori fanno ciò per ordinare gli eventi, per calcolare l'intervallo di tempo tra due date o per controllare la validità di un periodo di tempo.

## Come fare

Comparare due date in TypeScript avviene principalmente attraverso il metodo `getTime()` dell'oggetto `Date`. 

```TypeScript
let data1 = new Date(2021, 9, 1);
let data2 = new Date(2021, 9, 10);

if(data1.getTime() < data2.getTime()){
    console.log("Data1 è precedente a Data2");
}
else if(data1.getTime() > data2.getTime()){
    console.log("Data1 è successiva a Data2");
}
else {
    console.log("Le date sono uguali");
}
```

Il codice restituirà "Data1 è precedente a Data2" perché la prima data è il 1 ottobre 2021 e la seconda il 10 ottobre 2021. 

## Approfondimenti

Historicamente, JavaScript ha introdotto l'oggetto `Date` e i suoi metodi nel 1997, e TypeScript che estendi il JavaScript li ha presi in eredità. Oltre a `getTime()`, altri metodi utili per lavorare con le date includono `getDay()`, `getMonth()`, `getFullYear()`, tra gli altri. Una alternativa a `getTime()` sarebbe sottrarre direttamente le date, dato che JavaScript converte automaticamente le date in millisecondi quando si esegue un'operazione matematica.

```TypeScript
if(data1 < data2){
    console.log("Data1 è precedente a Data2");
}
```

Questa operazione può sembrare più pulita, ma può essere meno intuitiva per i programmatori inesperti perché non è immediatamente evidente che le date sono convertite in millisecondi. 

## Vedi anche

Per un approfondimento su come lavorare con Date in JavaScript (e di conseguenza TypeScript), consulta [la documentazione Mozilla JavaScript Date](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date). Se stai cercando una libreria per facilitare la manipolazione di date, dai un'occhiata a [Moment.js](http://momentjs.com/). Per alternative più moderne e leggere a Moment.js, esplora [Day.js](https://day.js.org/) o [date-fns](https://date-fns.org/).