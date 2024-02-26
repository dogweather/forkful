---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:44.879743-07:00
description: "Gli array associativi, o come sono pi\xF9 correttamente noti in JavaScript,\
  \ gli oggetti, ti permettono di mappare chiavi a valori. Questo \xE8 estremamente\u2026"
lastmod: '2024-02-25T18:49:41.653330-07:00'
model: gpt-4-0125-preview
summary: "Gli array associativi, o come sono pi\xF9 correttamente noti in JavaScript,\
  \ gli oggetti, ti permettono di mappare chiavi a valori. Questo \xE8 estremamente\u2026"
title: Utilizzo di array associativi
---

{{< edit_this_page >}}

## Cosa e Perché?

Gli array associativi, o come sono più correttamente noti in JavaScript, gli oggetti, ti permettono di mappare chiavi a valori. Questo è estremamente utile quando hai bisogno di una collezione di elementi che vuoi accedere tramite nomi specifici (chiavi) invece che indici numerici, rendendo il tuo codice più leggibile e flessibile.

## Come fare:

Creare e usare array associativi (oggetti) in JavaScript è semplice. Si definisce un oggetto con le parentesi graffe `{}`, e all'interno di queste, puoi definire un insieme di coppie chiave-valore. Le chiavi sono sempre stringhe, e i valori possono essere qualsiasi cosa: stringhe, numeri, array, persino altri oggetti.

```javascript
// Creazione di un array associativo
let userInfo = {
  name: "Alex",
  age: 30,
  email: "alex@example.com"
};

// Accesso agli elementi
console.log(userInfo.name); // Output: Alex
console.log(userInfo["email"]); // Output: alex@example.com

// Aggiunta di nuovi elementi
userInfo.job = "Developer";
userInfo["country"] = "Canada";

console.log(userInfo);
/* Output:
{
  name: "Alex",
  age: 30,
  email: "alex@example.com",
  job: "Developer",
  country: "Canada"
}
*/

// Eliminazione di un elemento
delete userInfo.age;
console.log(userInfo);
/* Output:
{
  name: "Alex",
  email: "alex@example.com",
  job: "Developer",
  country: "Canada"
}
*/
```

Come puoi vedere, accedere, aggiungere o eliminare elementi in un array associativo è abbastanza diretto e intuitivo.

## Approfondimento

Nel mondo di JavaScript, anche se spesso sentiamo il termine "array associativo", tecnicamente è un termine inappropriato perché JavaScript non ha veri array associativi come altri linguaggi (ad es., PHP). Ciò che JavaScript ha sono oggetti che servono uno scopo simile ma sono una costruzione più potente e flessibile.

Storicamente, gli array nei linguaggi di programmazione erano progettati per contenere una collezione di elementi, accessibili attraverso il loro indice numerico. Tuttavia, con l'evoluzione dello sviluppo software, è emersa la necessità di strutture dati più flessibili. Gli array associativi, o dizionari in altri linguaggi, sono state una risposta, consentendo l'accesso agli elementi attraverso chiavi arbitrarie.

L'approccio di JavaScript con gli oggetti come archivi chiave-valore offre una mescolanza di funzionalità. Permette di aggiungere, rimuovere e cercare proprietà (chiavi) per nome. JSON (JavaScript Object Notation) è una testimonianza dell'utilità di questa struttura, diventando lo standard de facto per lo scambio di dati sul web.

Mentre gli oggetti coprono la maggior parte delle esigenze per gli array associativi, nei casi in cui l'ordine delle chiavi o l'iterazione è importante, l'oggetto `Map` introdotto in ES6 offre un'alternativa migliore. Un `Map` mantiene l'ordine delle chiavi, accetta un'ampia gamma di tipi di dati come chiavi e include metodi utili per l'iterazione e il recupero delle dimensioni. Nonostante questi vantaggi, la sintassi dell'oggetto tradizionale rimane popolare per la sua semplicità e facilità d'uso in molti scenari comuni.
