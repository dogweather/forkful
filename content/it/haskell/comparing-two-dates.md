---
title:    "Haskell: Confronto di due date"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

##Perché
Ci sono molte ragioni per voler confrontare due date in un programma Haskell. Ad esempio, potresti voler sapere se una data è precedente o successiva rispetto all'altra, o magari vuoi fare qualche calcolo basato sulla differenza tra le due date. In ogni caso, il confronto di date è una funzionalità molto utile che può aiutarti a gestire meglio i dati nel tuo programma.

##Come fare
Per confrontare due date in Haskell, puoi utilizzare la funzione `compare` del modulo `Data.Time`. Qui di seguito c'è un esempio di codice che confronta due date e restituisce un valore di confronto:

```Haskell
import Data.Time

confrontaDate :: Day -> Day -> Ordering
confrontaDate data1 data2 = compare data1 data2
```

Nell'esempio sopra, la funzione `confrontaDate` riceve due valori di tipo `Day` (una data) e utilizza la funzione `compare` per confrontarli. Il risultato è del tipo `Ordering`, che può essere `LT` (per "meno di"), `GT` (per "più di") o `EQ` (per "uguale").

Ecco un altro esempio che mostra come utilizzare il valore di confronto per fare un controllo condizionale:

```Haskell
import Data.Time

controllaData :: Day -> Day -> String
controllaData data1 data2 =
    case compare data1 data2 of
        LT -> "La prima data è precedente alla seconda."
        GT -> "La prima data è successiva alla seconda."
        EQ -> "Le due date sono uguali."
```

In questo esempio, la funzione `controllaData` riceve due valori di tipo `Day` e utilizza il valore di confronto per restituire una stringa che indica la relazione tra le due date.

##Approfondimento
Se vuoi saperne di più sul confronto di date in Haskell, c'è un altro modulo molto utile che puoi utilizzare: `Data.Time.Calendar`. Questo modulo include diverse funzioni utili per lavorare con le date, come ad esempio `diffDays`, che calcola la differenza in giorni tra due date.

Ci sono anche altri moduli esterni che possono essere utili per il confronto di date, come ad esempio `Data.Ord` e `Data.Time.Format`.

Inoltre, puoi creare le tue funzioni personalizzate per confrontare date in base alle tue esigenze. Ad esempio, puoi scrivere una funzione che confronta solo il mese o solo l'anno di due date.

##Vedi anche
- [Documentazione ufficiale di Haskell sul modulo Data.Time](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/time-1.9.3/Data-Time.html)
- [Tutorial su come gestire le date in Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-dates-tom-lokhorst)
- [Libreria Data.Time.Calendar su Hackage](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Calendar.html)