---
title:    "Haskell: Convertire una data in una stringa"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Perché

Converting una data in una stringa può essere un'operazione utile quando si lavora con dati temporali in Haskell. Può essere richiesto per eseguire operazioni come l'ordinamento di date o la stampa di una data in un formato specificato. Vediamo come fare questo in Haskell.

## Come fare

Per convertire una data in una stringa, avremo bisogno del tipo di dato `Day` dal package `Data.Time`. Vediamo un esempio di come convertire una data nel formato `DD/MM/YYYY`:

```Haskell
import Data.Time.Format
import Data.Time.Calendar

dataToString :: Day -> String
dataToString date = formatTime defaultTimeLocale "%d/%m/%Y" date

```

Utilizzando la funzione `formatTime` e specificando il formato desiderato, possiamo facilmente ottenere una stringa della nostra data. Vediamo un esempio di input e output:

```Haskell
> let date = fromGregorian 2021 10 15    -- crea una data nel formato YYYY MM DD
> dataToString date
"15/10/2021"

```

Abbiamo ora una stringa contenente la nostra data. Possiamo anche utilizzare questa funzione per stampare una data in un determinato formato all'interno di una stringa, come ad esempio:

```Haskell
> "La data odierna è il " ++ dataToString date
"La data odierna è il 15/10/2021"

```

## Approfondimento

È importante notare che la funzione `formatTime` può essere utilizzata con diversi formati di data e ora, come ad esempio:

- `%a` per il nome abbreviato del giorno della settimana (es: "Mon")
- `%A` per il nome completo del giorno della settimana (es: "Monday")
- `%b` per il nome abbreviato del mese (es: "Jan")
- `%B` per il nome completo del mese (es: "January")
- `%d` per il giorno del mese con due cifre (es: "01")
- `%m` per il mese con due cifre (es: "01")
- `%Y` per l'anno con quattro cifre (es: "2021")

Puoi trovare ulteriori dettagli sulle opzioni dei formati nella documentazione ufficiale di `Data.Time.Format`.

## Per saperne di più

- [Documentazione ufficiale di `Data.Time.Format`](https://hackage.haskell.org/package/time/docs/Data-Time-Format.html)
- [Esempi di utilizzo della funzione `formatTime`](https://rosettacode.org/wiki/Date_format)
- [Introduzione a `Data.Time` in Haskell](https://github.com/oschware/haskell-intro/blob/master/Data.Time.md)