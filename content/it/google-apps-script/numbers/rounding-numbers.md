---
title:                "Arrotondamento dei numeri"
date:                  2024-02-01T22:00:37.058308-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arrotondamento dei numeri"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/google-apps-script/rounding-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e perché?

Arrotondare i numeri, un concetto fondamentale nella programmazione informatica, implica l'aggiustamento di un numero al suo intero più vicino o a un numero specificato di cifre decimali. I programmatori eseguono spesso l'arrotondamento per semplificare i numeri per la leggibilità umana o per soddisfare specifiche esigenze di calcolo, garantendo precisione e riducendo il carico computazionale.

## Come fare:

Google Apps Script, essendo un linguaggio basato su JavaScript, offre metodi standard per arrotondare i numeri. Ecco una ripartizione di tre tecniche comunemente utilizzate:

### Math.round()
Questa funzione arrotonda un numero all'intero più vicino.

```javascript
var number = 2.56;
var roundedNumber = Math.round(number); 
Logger.log(roundedNumber); // Mostra: 3
```

### Math.ceil()
Arrotonda un numero all'intero più vicino verso l'alto.

```javascript
var number = 2.56;
var roundedUp = Math.ceil(number); 
Logger.log(roundedUp); // Mostra: 3
```

### Math.floor()
Al contrario, arrotonda un numero all'intero più vicino verso il basso.

```javascript
var number = 2.56;
var roundedDown = Math.floor(number); 
Logger.log(roundedDown); // Mostra: 2
```

Per cifre decimali specifiche, puoi usare `.toFixed()`, che in realtà restituisce una stringa, o un approccio più sfumato per l'arrotondamento matematico:

```javascript
var number = 2.56789;
var fixedNumber = number.toFixed(2); 
Logger.log(fixedNumber); // Mostra: "2.57" (come stringa)

var preciseRound = Math.round(number * 100) / 100; 
Logger.log(preciseRound); // Mostra: 2.57
```

## Approfondimento

Arrotondare i numeri in Google Apps Script non si discosta molto da come si fa in altri ambienti JavaScript. Tuttavia, comprendere le differenze nei metodi di arrotondamento e il potenziale per questioni di aritmetica a virgola mobile è cruciale. Ad esempio, a causa del modo in cui i computer rappresentano i numeri in virgola mobile, non tutte le frazioni decimali possono essere rappresentate con perfetta precisione, portando a volte a risultati di arrotondamento inaspettati.

Storicamente, JavaScript (e per estensione, Google Apps Script) gestisce questo conformandosi allo standard IEEE 754, utilizzato da molti altri linguaggi di programmazione per l'aritmetica a virgola mobile. Questo standard definisce come i numeri sono arrotondati, garantendo coerenza attraverso varie piattaforme e linguaggi.

Mentre i metodi di arrotondamento diretti in Google Apps Script sono semplici e spesso sufficienti, applicazioni complesse o ad alta precisione potrebbero trarre vantaggio da librerie come decimal.js o big.js, progettate per gestire l'aritmetica di precisione arbitraria. Queste possono essere particolarmente utili quando si lavora con calcoli finanziari o scientifici dove l'accuratezza dei numeri arrotondati è fondamentale.

Ricorda, tuttavia, che l'uso di librerie esterne in Google Apps Script richiede di caricarle attraverso l'editor di script, il che può introdurre dipendenze o influenzare la prestazione del tuo script a seconda di come viene utilizzato. In molti casi, i metodi Math integrati sono completamente adeguati, ma per quei casi limite che richiedono precisione fino all'ennesimo grado, guardare oltre la libreria standard può essere necessario.
