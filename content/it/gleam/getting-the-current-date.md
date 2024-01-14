---
title:    "Gleam: Ottenere la data corrente."
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché
Solo 1-2 frasi per spiegare *perché* qualcuno dovrebbe voler ottenere la data corrente.

Se sei un programmatore Gleam, probabilmente sai che la data è un'informazione importante in molti programmi. Può essere utile per registrare quando vengono eseguite determinate azioni o per creare rapporti in applicazioni di monitoraggio dei dati. In questo articolo, ti mostrerò come ottenere la data corrente utilizzando il linguaggio di programmazione Gleam.

## Come Fare
Per ottenere la data corrente, è necessario utilizzare un modulo predefinito di Gleam chiamato `Time`. Questo modulo contiene funzioni utili per la gestione del tempo e delle date. Per prima cosa, importa il modulo nel tuo file di codice Gleam:

```Gleam
import Time
```

Successivamente, utilizza la funzione `now` del modulo per ottenere la data corrente:

```Gleam
let current_date = Time.now() 
```

La funzione `now` restituisce un record con le seguenti informazioni: anno, mese, giorno, ora, minuto, secondo e millisecondo. Puoi accedere a queste informazioni utilizzando la dot notation. Ad esempio, per ottenere il mese corrente, puoi utilizzare `current_date.month`.

```Gleam
let current_month = current_date.month
```

Puoi anche formattare la data secondo le tue esigenze utilizzando la funzione `format` del modulo `Time`. Questa funzione accetta una stringa di formato e restituisce la data formattata come una stringa.

```Gleam
let formatted_date = Time.format(current_date, "%d/%m/%Y") 
```

In questo esempio, abbiamo utilizzato la stringa di formato `%d/%m/%Y`, che rappresenta rispettivamente il giorno, il mese e l'anno. Puoi trovare una lista completa delle stringhe di formato disponibili nella documentazione di Gleam.

## Approfondimento
Ottenere la data corrente è un'operazione semplice, ma potrebbe essere più complessa se si vuole gestire fusi orari diversi o se si vuole ottenere una data storica. Per approfondire questo argomento, puoi consultare la documentazione ufficiale di Gleam sul modulo `Time`.

## Vedi Anche
- Documentazione ufficiale del modulo `Time`: https://gleam.run/documentation/stdlib/time
- Guida completa a Gleam: https://gleam.run/book/introduction
- Esempi di codice Gleam: https://github.com/gleam-lang/gleam/tree/main/examples