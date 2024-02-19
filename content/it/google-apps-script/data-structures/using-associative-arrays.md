---
aliases:
- /it/google-apps-script/using-associative-arrays/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:08.108194-07:00
description: "Gli array associativi, noti come oggetti in Google Apps Script (una\
  \ variante di JavaScript), consentono ai programmatori di creare collezioni di coppie\u2026"
lastmod: 2024-02-18 23:08:55.469945
model: gpt-4-0125-preview
summary: "Gli array associativi, noti come oggetti in Google Apps Script (una variante\
  \ di JavaScript), consentono ai programmatori di creare collezioni di coppie\u2026"
title: Utilizzo di array associativi
---

{{< edit_this_page >}}

## Cosa e Perché?

Gli array associativi, noti come oggetti in Google Apps Script (una variante di JavaScript), consentono ai programmatori di creare collezioni di coppie chiave-valore. Questa funzionalità risulta fondamentale per memorizzare e manipolare i dati in modo efficiente, specialmente quando si lavora con proprietà denominate dinamicamente o quando il modello di memorizzazione e accesso lineare di un array tradizionale non è sufficiente.

## Come fare:

In Google Apps Script, si creano e manipolano gli array associativi (oggetti) utilizzando le parentesi graffe `{}`, definendo al loro interno coppie chiave-valore. Le chiavi sono identificativi unici e i valori possono essere qualsiasi cosa, da stringhe e numeri a oggetti più complessi o funzioni. Ecco un esempio base:

```javascript
function createAssociativeArray() {
  var user = {
    name: "John Doe",
    age: 30,
    email: "johndoe@example.com"
  };

  // Accesso ai valori
  Logger.log(user.name); // Output: John Doe
  Logger.log(user["email"]); // Output: johndoe@example.com

  // Aggiunta di nuove coppie chiave-valore
  user.title = "Sviluppatore Software";
  user["country"] = "USA";

  Logger.log(user.title); // Output: Sviluppatore Software

  // Iterazione sulle coppie chiave-valore
  for (var key in user) {
    Logger.log(key + ': ' + user[key]);
  }
}
```

Un esempio di output per la parte di iterazione potrebbe essere questo:
```
name: John Doe
age: 30
email: johndoe@example.com
title: Sviluppatore Software
country: USA
```

Notare come sia possibile utilizzare sia la notazione a punti sia la notazione tra parentesi per accedere e impostare proprietà. La notazione tra parentesi risulta particolarmente utile quando si lavora con chiavi determinate dinamicamente o che includono caratteri non consentiti negli identificatori.

## Approfondimento

Gli array associativi sotto forma di oggetti sono stati una pietra miliare di JavaScript e, per estensione, Google Apps Script, riflettendo il suo meccanismo di ereditarietà basato su prototipi. A differenza di lingue con array associativi tradizionali o dizionari (ad es., il dict di Python), gli oggetti di Google Apps Script offrono un mezzo flessibile e potente per strutturare i dati, beneficiando della natura dinamica di JavaScript.

È importante notare, tuttavia, che la specifica ECMAScript 2015 ha introdotto gli oggetti `Map` e `Set`, offrendo una gestione delle collezioni associative più diretta con alcuni vantaggi rispetto agli oggetti, come il mantenimento dell'ordine di inserimento e prestazioni migliori per grandi dataset. Sebbene Google Apps Script supporti anche questi, la scelta tra l'utilizzo di oggetti o le più recenti strutture `Map`/`Set` dipende dalle specifiche esigenze e considerazioni di performance. Per la maggior parte dei compiti riguardanti gli array associativi, le implementazioni basate su oggetti tradizionali offrono un approccio familiare e versatile, ma è consigliabile esaminare nuove alternative man mano che la complessità dello script aumenta.
