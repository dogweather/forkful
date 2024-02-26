---
date: 2024-01-27 20:34:33.525945-07:00
description: "Generare numeri casuali in JavaScript \xE8 una tecnica usata per creare\
  \ imprevedibilit\xE0 nelle applicazioni, dai giochi che richiedono un comportamento\u2026"
lastmod: '2024-02-25T18:49:41.656027-07:00'
model: gpt-4-0125-preview
summary: "Generare numeri casuali in JavaScript \xE8 una tecnica usata per creare\
  \ imprevedibilit\xE0 nelle applicazioni, dai giochi che richiedono un comportamento\u2026"
title: Generazione di numeri casuali
---

{{< edit_this_page >}}

## Cosa e Perché?

Generare numeri casuali in JavaScript è una tecnica usata per creare imprevedibilità nelle applicazioni, dai giochi che richiedono un comportamento casuale dei nemici fino agli algoritmi di sicurezza che necessitano di casualità crittografica. Questa capacità è cruciale per sviluppare esperienze utente dinamiche e applicazioni sicure.

## Come Fare:

### Generazione di Base di Numeri Casuali

Il modo più diretto per generare un numero casuale in JavaScript è usare `Math.random()`. Questa funzione restituisce un numero a virgola mobile, pseudo-casuale nell'intervallo 0 (incluso) fino a 1 (escluso).

```javascript
let randomNumber = Math.random();
console.log(randomNumber);
```

### Generare un Numero Casuale Dentro un Intervallo

Spesso, si desidera ottenere un numero intero casuale all'interno di un intervallo specifico. Questo può essere raggiunto scalando e arrotondando l'output di `Math.random()`.

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

console.log(getRandomInt(1, 100));
```

### Numeri Casuali Crittograficamente Sicuri

Per applicazioni che richiedono un grado più elevato di casualità (ad es., operazioni crittografiche), si può utilizzare il metodo `crypto.getRandomValues()`. Questo fornisce casualità crittografica, a differenza dei numeri pseudo-casuali generati da `Math.random()`.

```javascript
(function generateSecureRandom() {
  let array = new Uint32Array(1);
  window.crypto.getRandomValues(array);
  console.log(array[0]);
})();
```

## Approfondimento

Storicamente, la generazione di numeri casuali in JavaScript si affidava esclusivamente alla funzione `Math.random()`. Sebbene conveniente per la maggior parte dei casi d'uso casuali, il suo algoritmo, tipicamente una variante di un generatore di numeri pseudo-casuali (PRNG) come Mersenne Twister, non fornisce sicurezza crittografica.

L'introduzione del Web Cryptography API ha portato il metodo `crypto.getRandomValues()`, offrendo un modo per generare numeri molto meno prevedibili e adatti per applicazioni sensibili alla sicurezza. Questo metodo attinge alle fonti di casualità del sistema operativo sottostante, come `/dev/random` su Unix/Linux, che sono più robuste e adatte per operazioni crittografiche.

È cruciale scegliere il metodo giusto per il compito da svolgere. `Math.random()` è sufficiente per le esigenze di base come giochi semplici, animazioni o qualsiasi caso in cui la qualità della casualità non sia critica. Tuttavia, per le funzionalità di sicurezza, come i token di reimpostazione della password o qualsiasi operazione crittografica, `crypto.getRandomValues()` è la scelta migliore a causa della sua superiore qualità della casualità.

Da notare, `Math.random()` genera numeri con un bias noto nella maggior parte delle implementazioni, il che significa che alcuni numeri sono più probabili di altri. Anche se questo bias è minimo e spesso impercettibile per le applicazioni generali, disqualifica `Math.random()` dall'uso in qualsiasi contesto crittografico o applicazioni dove la correttezza è fondamentale, come il gioco d'azzardo online.

In conclusione, mentre le funzioni integrate di JavaScript per generare numeri casuali coprono un vasto intervallo di esigenze, comprendere le differenze e i limiti di ciascun metodo è essenziale per la loro applicazione appropriata.
