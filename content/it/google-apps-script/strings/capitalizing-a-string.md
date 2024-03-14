---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:48:53.844534-07:00
description: "Capitalizzare una stringa comporta la modifica dell'input in modo che\
  \ il primo carattere sia maiuscolo mentre il resto rimanga in minuscolo, comunemente\u2026"
lastmod: '2024-03-13T22:44:42.935676-06:00'
model: gpt-4-0125-preview
summary: "Capitalizzare una stringa comporta la modifica dell'input in modo che il\
  \ primo carattere sia maiuscolo mentre il resto rimanga in minuscolo, comunemente\u2026"
title: Mettere in Maiuscolo una Stringa
---

{{< edit_this_page >}}

## Cosa & Perché?

Capitalizzare una stringa comporta la modifica dell'input in modo che il primo carattere sia maiuscolo mentre il resto rimanga in minuscolo, comunemente utilizzato per formattare nomi o titoli. I programmatori lo fanno per garantire la coerenza dei dati e migliorare la leggibilità all'interno delle interfacce utente o dei documenti.

## Come fare:

Google Apps Script, essendo basato su JavaScript, consente diversi metodi per capitalizzare una stringa, sebbene senza una funzione integrata. Ecco un paio di esempi concisi:

**Metodo 1: Usando charAt() e slice()**

```javascript
function capitalizeString(inputString) {
  if (!inputString) return '';
  return inputString.charAt(0).toUpperCase() + inputString.slice(1).toLowerCase();
}

// Esempio di utilizzo
let result = capitalizeString('hello, world');
console.log(result);  // Output: Hello, world
```

**Metodo 2: Usando una Regex**

Per coloro che preferiscono una soluzione basata su regex per gestire i casi limite in modo più elegante:

```javascript
function capitalizeStringRegex(inputString) {
  return inputString.toLowerCase().replace(/^\w/, c => c.toUpperCase());
}

// Esempio di utilizzo
let result = capitalizeStringRegex('hello, world');
console.log(result);  // Output: Hello, world
```

Entrambi i metodi garantiscono che il primo carattere della stringa sia maiuscolo e il resto minuscolo, adatti per una varietà di applicazioni inclusi ma non limitati alla manipolazione di Google Sheets o alla modifica di documenti tramite Apps Script.

## Approfondimento

Capitalizzare le stringhe in Google Apps Script è semplice, sfruttando le potenti capacità di manipolazione delle stringhe di JavaScript. Storicamente, linguaggi come Python offrono metodi integrati come `.capitalize()` per ottenere questo, ponendo un piccolo passo aggiuntivo per i programmatori JavaScript e Apps Script. Tuttavia, l'assenza di una funzione integrata in JavaScript/Google Apps Script incoraggia flessibilità e una comprensione più profonda delle tecniche di manipolazione delle stringhe.

Per scenari complessi, come capitalizzare ogni parola in una stringa (Maiuscolo iniziale), i programmatori potrebbero combinare metodi regex con funzioni `split()` e `map()` per elaborare ogni parola individualmente. Sebbene Google Apps Script non fornisca un metodo diretto per la capitalizzazione delle stringhe, l'uso dei metodi di manipolazione delle stringhe di JavaScript esistenti offre un'ampia flessibilità, consentendo agli sviluppatori di gestire le stringhe in modo efficiente secondo le loro specifiche esigenze.

Nei casi in cui la performance e l'efficienza sono di massima importanza, vale la pena notare che la manipolazione diretta delle stringhe potrebbe essere più performante rispetto alla regex, specialmente per stringhe più lunghe o operazioni all'interno di grandi cicli. Tuttavia, per la maggior parte delle applicazioni pratiche all'interno di Google Apps Script, entrambi gli approcci forniscono soluzioni affidabili.
