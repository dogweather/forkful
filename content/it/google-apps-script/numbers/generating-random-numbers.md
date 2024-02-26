---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:00.152317-07:00
description: "Generare numeri casuali \xE8 un compito fondamentale nella programmazione\
  \ che viene utilizzato per una miriade di applicazioni, come simulazioni, giochi\
  \ e\u2026"
lastmod: '2024-02-25T18:49:40.879982-07:00'
model: gpt-4-0125-preview
summary: "Generare numeri casuali \xE8 un compito fondamentale nella programmazione\
  \ che viene utilizzato per una miriade di applicazioni, come simulazioni, giochi\
  \ e\u2026"
title: Generare numeri casuali
---

{{< edit_this_page >}}

## Cosa & Perché?

Generare numeri casuali è un compito fondamentale nella programmazione che viene utilizzato per una miriade di applicazioni, come simulazioni, giochi e sistemi di sicurezza. I programmatori impiegano questa tecnica in Google Apps Script per introdurre variabilità, testare scenari e aggiungere imprevedibilità alle loro applicazioni all'interno dell'ecosistema Google, inclusi Fogli, Documenti e Moduli.

## Come fare:

In Google Apps Script, puoi generare numeri casuali utilizzando la funzione `Math.random()`, simile a JavaScript. Questa funzione restituisce un numero pseudo-casuale a virgola mobile nell'intervallo da 0 (incluso) a 1 (escluso). Per adattare questi numeri a vari casi d'uso, come generare interi all'interno di un intervallo specifico, potrebbe essere necessario eseguire calcoli aggiuntivi.

### Generare un Numero Casuale di Base

Per generare un semplice numero casuale e registrarlo nella console:

```javascript
function generateRandomNumber() {
  var randomNumber = Math.random();
  Logger.log(randomNumber);
}
```
*Output campione:* `0.1234567890123456`

### Generare un Intero all'interno di un Intervallo Specifico

Per generare un intero casuale tra due valori (`min` e `max`), inclusivi:

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  var randomNumber = Math.floor(Math.random() * (max - min + 1)) + min;
  Logger.log(randomNumber);
  return randomNumber;
}

// Esempio:
getRandomInt(1, 10);
```
*Output campione*: `7`

Ricorda, la funzione `Math.ceil()` viene utilizzata per arrotondare per eccesso il valore minimo e `Math.floor()` viene usato per arrotondare per difetto il valore massimo, garantendo che il numero casuale rientri nell'intervallo specificato.

## Approfondimento

Il meccanismo per generare numeri casuali in Google Apps Script, e in effetti nella maggior parte dei linguaggi di programmazione, utilizza un generatore di numeri pseudo-casuali (PRNG). Questa tecnica è deterministica e si basa su un valore iniziale, noto come seme, per produrre una sequenza di numeri che appare casuale. Sebbene sia sufficiente per molte applicazioni, è importante notare che i numeri pseudo-casuali potrebbero non essere appropriati dove è richiesta un'alta sicurezza o vera casualità, come nelle applicazioni crittografiche.

La vera casualità può essere ottenuta tramite generatori di numeri casuali hardware o servizi che generano casualità da fenomeni naturali. Tuttavia, per la maggior parte delle esigenze di scripting quotidiane in Google Apps Script, `Math.random()` è sufficiente.

Storicamente, la ricerca di tecniche di generazione di numeri casuali più efficaci ha portato allo sviluppo di vari algoritmi, con esempi degni di nota che sono il Mersenne Twister e il Generatore Congruenziale Lineare (LCG). Tuttavia, dato l'alto livello di astrazione in Google Apps Script, la maggior parte degli utenti non avrà bisogno di implementare direttamente questi algoritmi, ma comprendere i principi sottostanti può aiutare ad apprezzare l'importanza e i limiti della generazione di numeri casuali nei tuoi script.
