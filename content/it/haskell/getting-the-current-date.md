---
title:    "Haskell: Ottenere la data corrente"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Se sei un appassionato di programmazione o stai imparando il linguaggio Haskell, potresti voler sapere come ottenere la data corrente nel tuo codice. Ci sono molte ragioni per cui potresti voler usare questa funzione, ad esempio per registrare gli orari delle tue attività o per gestire gli eventi in base alla data corrente.

## Come fare

Per ottenere la data corrente in Haskell, puoi utilizzare la funzione `getCurrentTime` dalla libreria `Data.Time`. Ecco un esempio di codice:

```Haskell
import Data.Time

main = do
  currentTime <- getCurrentTime
  print currentTime
```

Questo codice importerà la libreria necessaria e poi utilizzerà la funzione `getCurrentTime` per ottenere la data e l'ora correnti. Il risultato verrà quindi stampato a schermo.

L'output sarà qualcosa del genere:

```
2021-10-02 14:30:00.123456789 UTC
```

## Approfondimento

Esistono diversi modi in cui puoi ottenere la data corrente in Haskell, a seconda del formato desiderato. Ad esempio, potresti voler ottenere solo il giorno, il mese o l'anno corrente invece della data completa come nell'esempio sopra.

Inoltre, la funzione `getCurrentTime` restituisce un'informazione più precisa di quella mostrata sopra, ma per motivi di spazio e semplicità, abbiamo troncato il risultato nell'esempio. Se vuoi, puoi approfondire la documentazione della libreria `Data.Time` per scoprire tutte le opzioni disponibili e come utilizzarle correttamente.

## Vedi anche

- [Documentazione ufficiale di Data.Time](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Clock.html)
- [Tutorial su come utilizzare le date in Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/time)