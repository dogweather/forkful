---
aliases:
- /it/google-apps-script/concatenating-strings/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:50:01.527162-07:00
description: "Concatenare le stringhe significa combinare due o pi\xF9 stringhe in\
  \ una singola stringa. I programmatori lo fanno per costruire dinamicamente messaggi,\
  \ URL\u2026"
lastmod: 2024-02-18 23:08:55.468944
model: gpt-4-0125-preview
summary: "Concatenare le stringhe significa combinare due o pi\xF9 stringhe in una\
  \ singola stringa. I programmatori lo fanno per costruire dinamicamente messaggi,\
  \ URL\u2026"
title: Concatenazione di stringhe
---

{{< edit_this_page >}}

## Cosa & Perché?

Concatenare le stringhe significa combinare due o più stringhe in una singola stringa. I programmatori lo fanno per costruire dinamicamente messaggi, URL o qualsiasi forma di testo che richiede una miscela di contenuto statico e variabile.

## Come fare:

In Google Apps Script, che si basa su JavaScript, ci sono diversi modi per concatenare le stringhe. Ecco alcuni metodi comuni:

### Utilizzando l'operatore più (`+`):

```javascript
var firstName = "John";
var lastName = "Doe";
var fullName = firstName + " " + lastName;
Logger.log(fullName); // Output: John Doe
```

### Utilizzando il metodo `concat()`:

```javascript
var string1 = "Hello";
var string2 = "World";
var combinedString = string1.concat(" ", string2);
Logger.log(combinedString); // Output: Hello World
```

### Utilizzando i template literal (backticks):

Questo è un modo moderno e flessibile per concatenare le stringhe, che consente di incorporare facilmente espressioni all'interno delle stringhe.

```javascript
var language = "Google Apps Script";
var message = `Imparare ${language} è divertente!`;
Logger.log(message); // Output: Imparare Google Apps Script è divertente!
```

Ciascuno di questi metodi ha i suoi casi d'uso, e la scelta tra di loro dipende tipicamente dai requisiti di leggibilità e dalla complessità delle stringhe da concatenare.

## Approfondimento

La concatenazione di stringhe è un aspetto fondamentale non solo di Google Apps Script ma di molti linguaggi di programmazione. Storicamente, la concatenazione di stringhe veniva spesso eseguita utilizzando l'operatore più o funzioni/metodi specializzati come `concat()`. Tuttavia, con l'introduzione dei template literal in ECMAScript 2015 (ES6), che Google Apps Script supporta, gli sviluppatori hanno ottenuto un modo più potente e intuitivo di gestire le stringhe.

I template literal non solo semplificano la sintassi per incorporare espressioni all'interno delle stringhe ma supportano anche stringhe su più righe senza la necessità di caratteri di nuova linea espliciti. Questo riduce il potenziale per errori e migliora la leggibilità del codice, specialmente quando si gestiscono stringhe complesse o quando si sostituiscono più variabili in un template di testo.

Sebbene l'operatore `+` e il metodo `concat()` siano ancora ampiamente utilizzati e supportati per la compatibilità con le versioni precedenti e la semplicità in scenari più semplici, i template literal offrono un'alternativa moderna ed espressiva che è spesso considerata superiore per la concatenazione di stringhe, particolarmente quando la leggibilità e la manutenibilità sono di preoccupazione.

Tuttavia, è importante scegliere il metodo che si adatta meglio al contesto specifico e ai requisiti del proprio progetto, considerando fattori come la compatibilità dell'ambiente di destinazione (anche se raramente è un problema con Google Apps Script), le implicazioni sulle prestazioni (minime per la maggior parte delle applicazioni) e la familiarità del team di sviluppo con le funzionalità moderne di JavaScript.
