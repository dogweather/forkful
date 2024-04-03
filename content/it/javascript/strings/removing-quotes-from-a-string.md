---
date: 2024-01-26 03:40:23.871094-07:00
description: "Rimuovere le virgolette da una stringa significa sbarazzarsi di quei\
  \ fastidiosi segni di punteggiatura che possono creare problemi con il tuo codice,\u2026"
lastmod: '2024-03-13T22:44:43.800059-06:00'
model: gpt-4-0125-preview
summary: Rimuovere le virgolette da una stringa significa sbarazzarsi di quei fastidiosi
  segni di punteggiatura che possono creare problemi con il tuo codice, specialmente
  quando si analizzano dati o si costruiscono oggetti JSON.
title: Rimuovere le virgolette da una stringa
weight: 9
---

## Come fare:
Immagina di avere una stringa racchiusa tra virgolette doppie, come `"\"Ciao, Mondo!\""` e vuoi il testo puro, senza virgolette. Ecco un breve frammento di JavaScript per liberare la tua stringa da quelle catene di virgolette:

```javascript
let quotedString = "\"Ciao, Mondo!\"";
let unquotedString = quotedString.replace(/^"|"$/g, '');
console.log(unquotedString); // Output: Ciao, Mondo!
```

E se stai trattando con virgolette singole? Basta regolare un po' l'espressione regolare:

```javascript
let singleQuotedString = "'Ciao, Mondo!'";
let unquotedString = singleQuotedString.replace(/^'|'$/g, '');
console.log(unquotedString); // Output: Ciao, Mondo!
```

O se la tua stringa ha un mix di entrambi? Nessun problema:

```javascript
let mixedQuotedString = "\"'Ciao, Mondo!'\"";
let unquotedString = mixedQuotedString.replace(/^["']|["']$/g, '');
console.log(unquotedString); // Output: 'Ciao, Mondo!'
```

## Approfondimento
Prima che JSON prendesse il sopravvento, sfuggire le virgolette era un far west di backslash e stratagemmi. I primi linguaggi di programmazione non sempre andavano d'accordo con le virgolette, il che significava un sacco di manipolazione manuale delle stringhe. Ora, con formati di dati standardizzati, rimuovere le virgolette è spesso questione di pulizia degli input prima che vengano elaborati come JSON o di memorizzazione di testo senza conflitti di formattazione.

Alternative a `.replace()`? Certo! Potresti dividere e unire una stringa sulle virgolette, usare slice se sei certo della posizione delle tue virgolette, o anche match regex per estrarre il testo necessario. Tutto dipende dal contesto.

Ma non dimenticare i casi limite: virgolette dentro le virgolette, virgolette escape e caratteri internazionali. Pensa alla tua stringa come a un potenziale campo minato di eccezioni e procedi con cautela. I motori JavaScript moderni sono ottimizzati per gestire efficientemente le operazioni regex, quindi generalmente sono la scelta prediletta, ma vale sempre la pena controllare le prestazioni per compiti di elaborazione di dati pesanti.

## Vedi anche
Approfondisci la manipolazione delle stringhe e regex:

- Mozilla Developer Network su String.replace(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- Regex101 per testare i tuoi schemi regex: https://regex101.com/
- JSON.org per capire perché abbiamo a che fare con così tante virgolette nello sviluppo web moderno: http://json.org/
