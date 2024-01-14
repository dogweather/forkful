---
title:    "Gleam: Unire stringhe"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

La concatenazione di stringhe è un'operazione fondamentale nella programmazione che consente di combinare diverse stringhe in una sola. Questo può essere utile per creare messaggi personalizzati, creare output dinamici e altro ancora. Continua a leggere per scoprire come farlo in Gleam!

## Come fare

Per concatenare stringhe in Gleam, possiamo utilizzare l'operatore `~` che unisce due stringhe. Ad esempio:

```Gleam
let nome = "Marco"
let saluto = "Ciao " ~ nome
```

Questa concatenazione produrrà la stringa "Ciao Marco". Possiamo anche unire più di due stringhe, come in questo esempio:

```Gleam
let nome = "Marco"
let cognome = "Rossi"
let saluto = "Ciao " ~ nome ~ " " ~ cognome
```

Questa volta otterremo la stringa "Ciao Marco Rossi". Possiamo anche concatenare stringhe e variabili di altri tipi di dati, come numeri:

```Gleam
let nome = "Anna"
let contatore = 3
let saluto = nome ~ ", hai " ~ contatore ~ " nuovi messaggi."
```

Questo codice produrrà la stringa "Anna, hai 3 nuovi messaggi." Come puoi vedere, la concatenazione di stringhe è un modo semplice ma potente per creare output personalizzati nei tuoi programmi Gleam.

## Approfondimento

In Gleam, le stringhe sono immutabili, il che significa che non possono essere modificate dopo essere state create. Ciò significa che ogni volta che concateniamo una stringa, in realtà stiamo creando una nuova stringa invece di modificare quella esistente. Questo è importante da tenere presente quando si lavora con stringhe più grandi o in situazioni in cui le prestazioni sono cruciali.

Un altro concetto importante da comprendere è la conversione implicita nel concatenare tipi diversi di dati. Ad esempio, se proviamo a concatenare una stringa con un numero, Gleam effettuerà automaticamente la conversione del numero in una stringa prima di effettuare la concatenazione. Questo può essere utile, ma può anche portare a errori se non si ha la consapevolezza di questo comportamento.

Inoltre, esistono anche alcune funzioni utili nella libreria standard di Gleam per la manipolazione delle stringhe, come `length` per ottenere la lunghezza di una stringa e `to_upper` per convertire una stringa in maiuscolo.

## Vedi anche

-  La [documentazione ufficiale di Gleam](https://gleam.run/documentation) per ulteriori informazioni sulla concatenazione di stringhe e altre funzionalità del linguaggio.
- La [Guida Gleam di programmazione funzionale](https://gleam.run/book) per approfondire la conoscenza di Gleam e della programmazione funzionale in generale.
- Il [blog ufficiale di Gleam](https://gleam.run/blog) per rimanere aggiornati sulle ultime novità e progetti inerenti a questo linguaggio di programmazione innovativo.