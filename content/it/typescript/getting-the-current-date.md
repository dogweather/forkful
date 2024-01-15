---
title:                "Ottenere la data corrente"
html_title:           "TypeScript: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Perché dovresti preoccuparti di ottenere la data corrente? Beh, ci sono molte ragioni valide. Innanzitutto, potresti voler mostrare la data attuale all'utente in una pagina web o un'applicazione. Oppure potresti avere la necessità di eseguire alcune operazioni in base alla data attuale. In sintesi, ottenere la data corrente è un'abilità molto utile da avere nel tuo kit di strumenti di sviluppo.

## Come

Per ottenere la data corrente in TypeScript, possiamo utilizzare l'oggetto `Date` incluso nella libreria standard di JavaScript. Ecco un esempio di codice che mostra come fare:

```TypeScript
const oggi: Date = new Date();
console.log(oggi);
```

Eseguendo questo codice, vedrai l'output simile a questo:

```
Tue Jul 27 2021 12:28:18 GMT+0200 (Central European Summer Time)
```

Come puoi vedere, l'oggetto `Date` contiene tutte le informazioni sulla data e l'ora attuali. Ma cosa succede se vogliamo ottenere solo la data senza l'ora? Possiamo utilizzare alcuni dei metodi disponibili nell'oggetto `Date` per ottenere solo gli elementi che ci interessano. Ad esempio, se vogliamo ottenere il giorno, il mese e l'anno correnti, possiamo fare in questo modo:

```TypeScript
const oggi: Date = new Date();
const giorno: number = oggi.getDate();
const mese: number = oggi.getMonth() + 1; // Gennaio è 0, quindi aggiungiamo 1
const anno: number = oggi.getFullYear();

console.log(`${giorno}/${mese}/${anno}`);
```

L'output di questo codice sarà:

```
27/7/2021
```

## Deep Dive

Ora che abbiamo visto come ottenere la data corrente in TypeScript, andiamo un po' più in profondità. L'oggetto `Date` in realtà è molto più potente di quanto sembri. Offre una varietà di metodi e proprietà che possono essere utilizzati per manipolare e formattare le date. Ad esempio, possiamo utilizzare il metodo `toLocaleString()` per ottenere la data corrente in una lingua specifica. Inoltre, possiamo anche effettuare operazioni matematiche con le date utilizzando i metodi `getTime()` e `setTime()`. Se vuoi saperne di più su come utilizzare l'oggetto `Date` in TypeScript, ti consiglio di fare una ricerca su internet o di consultare la documentazione ufficiale di TypeScript.

## Vedi anche

- [Documentazione ufficiale di TypeScript sull'oggetto `Date`](https://www.typescriptlang.org/docs/handbook/datetime.html)
- [Guida su come utilizzare l'oggetto `Date` in JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Altro articolo su come ottenere la data corrente in TypeScript](https://dev.to/auroratide/how-to-get-the-current-date-and-time-using-typescript-41nk)