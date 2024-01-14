---
title:                "TypeScript: Confronto tra due date"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché
Comparare due date può essere un'operazione utile in molte situazioni di programmazione, come ad esempio nel controllo di scadenze o nel confronto di date tra database.

## Come Fare
Per iniziare, si deve prima dichiarare due variabili contenenti le due date da confrontare. Queste possono essere di tipo "Date" o essere convertite in tale tipo utilizzando il metodo ```new Date('data')```. Successivamente, si può utilizzare l'operatore di confronto ```>``` per verificare quale delle due date sia più grande o l'operatore ```===``` per controllare se le date siano uguali.

```TypeScript
let data1: Date = new Date('2021-05-10');
let data2: Date = new Date('2021-05-15');

console.log(data2 > data1); // output: true
console.log(data1 === data2); //output: false
```

## Approfondimento
La comparazione di date può diventare più complessa quando si considerano anche gli orari. In questo caso, è possibile utilizzare i metodi ```getDate()```, ```getMonth()```, ```getFullYear()```, ```getHours()```, ```getMinutes()``` e ```getSeconds()``` per ottenere i valori specifici delle date e confrontarli in modo più preciso.

```TypeScript
let data1: Date = new Date('2021-05-10 10:00');
let data2: Date = new Date('2021-05-10 12:00');

let ore1: number = data1.getHours();
let ore2: number = data2.getHours();
if (ore1 > ore2) {
  console.log('La data1 è successiva alla data2'); // output: La data1 è successiva alla data2
}
```

## Vedi Anche
- [Documentazione ufficiale di TypeScript](https://www.typescriptlang.org/docs/)
- [Tutorial su come gestire le date in TypeScript](https://www.digitalocean.com/community/tutorials/typescript-italian-date)
- [Esempi pratici di confronto tra date in TypeScript](https://www.javatpoint.com/typescript-date-comparison)