---
title:                "Haskell: Converting una data in una stringa"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Perché

Convertire una data in una stringa è un'operazione molto comune e utile in programmazione. Ciò consente di visualizzare la data in un formato facilmente leggibile per gli utenti, come ad esempio "1 gennaio 2021", anziché un valore numerico come "1/1/21".

# Come fare

Per convertire una data in una stringa in Haskell, è possibile utilizzare la funzione `formatTime` del modulo `Data.Time.Format`. Questa funzione richiede due argomenti: un formato di output e una data da convertire. Ad esempio, per convertire la data corrente in una stringa nel formato "DD/MM/YYYY", è possibile utilizzare il seguente codice:

```
import Data.Time.Format

main = do
  current <- getCurrentTime
  let output = formatTime defaultTimeLocale "%d/%m/%Y" current
  putStrLn output
```

L'output sarà "01/01/2021".

# Approfondimento

La funzione `formatTime` accetta una vasta gamma di formati di data, come ad esempio "%A, %d %B %Y" per ottenere un output come "Venerdì, 01 gennaio 2021". Inoltre, è possibile specificare anche altre localizzazioni, come la lingua e il fuso orario, utilizzando il parametro `locale` della funzione.

Inoltre, è importante tenere conto della gestione della data nel proprio ambiente di sviluppo, poiché il formato della data potrebbe variare a seconda del sistema operativo utilizzato.

# Vedi anche

- [Documentazione di `Data.Time.Format`](https://hackage.haskell.org/package/time/docs/Data-Time-Format.html)
- [Tutorial su come gestire le date in Haskell](https://guide.aelve.com/haskell/time/1-date-and-time)