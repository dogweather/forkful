---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:39.634245-07:00
description: "I numeri complessi, rappresentati come una combinazione di unit\xE0\
  \ reali e immaginarie (ad es., 3 + 4i), sono fondamentali in vari problemi computazionali,\u2026"
lastmod: '2024-03-13T22:44:42.947935-06:00'
model: gpt-4-0125-preview
summary: "I numeri complessi, rappresentati come una combinazione di unit\xE0 reali\
  \ e immaginarie (ad es., 3 + 4i), sono fondamentali in vari problemi computazionali,\u2026"
title: Lavorare con i numeri complessi
weight: 14
---

## Cosa e perché?
I numeri complessi, rappresentati come una combinazione di unità reali e immaginarie (ad es., 3 + 4i), sono fondamentali in vari problemi computazionali, specialmente in ingegneria, fisica e matematica applicata. Imparare a manipolare questi numeri in Google Apps Script permette ai programmatori di estendere le loro capacità nel calcolo scientifico, nell'elaborazione dei segnali e oltre.

## Come fare:
Google Apps Script non ha un supporto integrato per i numeri complessi, rendendo necessaria l'implementazione di funzionalità personalizzate. Di seguito è riportata una struttura di base per gestire i numeri complessi, inclusi l'addizione, la sottrazione e la moltiplicazione.

```javascript
// Definire un costruttore per i numeri complessi
function Complex(reale, immag) {
  this.reale = reale;
  this.immag = immag;
}

// Metodo per aggiungere due numeri complessi
Complex.prototype.add = function(other) {
  return new Complex(this.reale + other.reale, this.immag + other.immag);
};

// Metodo per sottrarre due numeri complessi
Complex.prototype.subtract = function(other) {
  return new Complex(this.reale - other.reale, this.immag - other.immag);
};

// Metodo per moltiplicare due numeri complessi
Complex.prototype.multiply = function(other) {
  return new Complex(
    this.reale * other.reale - this.immag * other.immag,
    this.reale * other.immag + this.immag * other.reale
  );
};

// Esempio di utilizzo
var num1 = new Complex(3, 4);
var num2 = new Complex(1, 2);

// Sommare due numeri complessi
var somma = num1.add(num2);
console.log(`Somma: ${somma.reale} + ${somma.immag}i`); // Somma: 4 + 6i

// Sottrarre due numeri complessi
var differenza = num1.subtract(num2);
console.log(`Differenza: ${differenza.reale} + ${differenza.immag}i`); // Differenza: 2 + 2i

// Moltiplicare due numeri complessi
var prodotto = num1.multiply(num2);
console.log(`Prodotto: ${prodotto.reale} + ${prodotto.immag}i`); // Prodotto: -5 + 10i
```

## Approfondimento:
Il concetto di numeri complessi risale al XVI secolo, ma fu il lavoro di matematici come Eulero e Gauss a consolidare il loro posto nella matematica. Nonostante la loro utilità, i numeri complessi non sono supportati direttamente in JavaScript o, per estensione, in Google Apps Script. La mancanza di supporto nativo significa che le operazioni sui numeri complessi devono essere implementate manualmente, come dimostrato. Sebbene ciò fornisca una buona opportunità di apprendimento e una funzionalità sufficiente per le esigenze di base, per lavori computazionali intensivi che richiedono numeri complessi, si potrebbe considerare l'uso di altri ambienti di programmazione più adatti al calcolo matematico, come Python con NumPy, che offrono operazioni integrate e altamente ottimizzate per gestire i numeri complessi. Tuttavia, capire e implementare le operazioni di base in Google Apps Script è un esercizio utile per coloro che cercano di ampliare le loro competenze di programmazione e applicarle in una vasta gamma di contesti.
