---
title:                "Ruby: Calcolare una data nel futuro o nel passato"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché
Calcolare una data nel futuro o nel passato può essere molto utile in molte situazioni di programmazione, come ad esempio la pianificazione di eventi o la gestione di scadenze.

## Come Fare
Per calcolare una data in Ruby, è necessario utilizzare la classe `Time` e il suo metodo `strftime`. Di seguito è riportato un esempio di codice per ottenere la data odierna:

```Ruby
Time.now.strftime("%d/%m/%Y")
```

Questo verrà visualizzato come `01/09/2021` per il 1° settembre 2021. Per calcolare una data in futuro o in passato, è possibile utilizzare gli operatori matematici come `+` e `-` per aggiungere o sottrarre giorni, mesi o anni dalla data attuale. Ad esempio, il seguente codice restituirà la data del 1° novembre 2021:

```Ruby
(Time.now + (31*24*60*60)).strftime("%d/%m/%Y")
```

In questo esempio, moltiplichiamo il numero di giorni (31) per il numero di secondi in un giorno (24*60*60) e lo aggiungiamo alla data attuale.

## Approfondimento
Esistono molti altri metodi per effettuare calcoli di data in Ruby, come ad esempio l'utilizzo della classe `DateTime`. Inoltre, è possibile gestire fusi orari e date non standard utilizzando la gemma Ruby `tzinfo`. Per ulteriori informazioni e opzioni per il calcolo di date nel futuro o nel passato, è possibile consultare la documentazione di Ruby o esplorare ulteriori risorse online.

## Vedi Anche
- Documentazione di Ruby sulle classi `Time` e `DateTime`
- Documentazione della gemma Ruby `tzinfo`

Grazie per aver letto questo post sul calcolo di date in Ruby. Speriamo che sia stato utile per la tua programmazione!