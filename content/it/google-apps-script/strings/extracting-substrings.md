---
title:                "Estrazione di sottostringhe"
aliases: - /it/google-apps-script/extracting-substrings.md
date:                  2024-02-01T21:52:54.882865-07:00
model:                 gpt-4-0125-preview
simple_title:         "Estrazione di sottostringhe"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/google-apps-script/extracting-substrings.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cos'è & Perché?

Estrarre sottosequenze di stringhe implica prendere una porzione di una stringa - creando essenzialmente una nuova stringa a partire da una esistente. I programmatori fanno ciò per una miriade di ragioni, incluse l'analisi dei dati, la manipolazione di testo per interfacce utente o l'elaborazione dell'input per varie applicazioni, rendendo l'estrazione di sottosequenze uno strumento versatile in qualsiasi arsenale di scripting.

## Come fare:

In Google Apps Script, che si basa su JavaScript moderno, l'estrazione di sottosequenze può essere raggiunta attraverso vari metodi, inclusi `substring()`, `substr()`, e `slice()`. Ognuno ha le sue sfumature, ma tutti servono allo scopo di estrarre caratteri specificati da una stringa.

```javascript
// Esempio usando substring()
var str = "Hello, world!";
var result = str.substring(0, 5);
console.log(result); // Output: Hello

// Esempio usando substr()
var resultSubstr = str.substr(7, 5);
console.log(resultSubstr); // Output: world

// Esempio usando slice()
var resultSlice = str.slice(-6);
console.log(resultSlice); // Output: world!
```

Ciascun metodo prende due argomenti: la posizione di inizio e, ad eccezione di `slice()` che può accettare indici negativi per iniziare dalla fine, la posizione finale o il numero di caratteri da estrarre. Vale la pena notare che la stringa originale rimane invariata dopo queste operazioni, poiché restituiscono nuovi valori di stringa.

## Approfondimento

Storicamente, i metodi JavaScript per estrarre sottosequenze sono stati una fonte di confusione a causa dei loro nomi simili e funzionalità. Tuttavia, in Google Apps Script e nel JavaScript moderno, `substring()` e `slice()` sono i più usati, con `substr()` considerato deprecato. Questo è importante da notare per chi scrive codice a prova di futuro.

La principale differenza tra `substring()` e `slice()` è in come gestiscono gli indici negativi; `substring()` tratta gli indici negativi come 0, mentre `slice()` può accettare un indice negativo per iniziare l'estrazione dalla fine della stringa. Ciò rende `slice()` particolarmente utile per casi in cui la lunghezza esatta della stringa potrebbe non essere nota o quando si necessita di estrarre dalla fine.

Quando si decide quale metodo utilizzare per l'estrazione di sottosequenze, la scelta spesso si riduce ai requisiti specifici dell'operazione (ad es., se è vantaggioso gestire indici negativi) e agli standard di codifica personali o di team. Sebbene non ci sia una pratica migliore universale, comprendere le differenze sottili e le implicazioni sulle prestazioni può aiutare a prendere una decisione informata.
