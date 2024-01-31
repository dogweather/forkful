---
title:                "Utilizzo di array associativi"
date:                  2024-01-30T19:11:28.680730-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo di array associativi"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/using-associative-arrays.md"
changelog:
  - 2024-01-30, dogweather, reviewed
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Gli array associativi, o dizionari, in Haskell riguardano principalmente la mappatura di chiavi a valori per una ricerca veloce e una gestione efficiente dei dati. I programmatori li utilizzano per gestire collezioni di elementi accoppiati, dove cercare un elemento è molto più semplice, rispetto alle liste.

## Come fare:

Haskell non ha array associativi pronti all'uso allo stesso modo di alcuni altri linguaggi, ma offre una potente libreria standard chiamata `Data.Map` per lavorare con coppie chiave-valore. Rimbocchiamoci le maniche e vediamo come usarli!

Prima di tutto, assicurati di importarla:
```Haskell
import qualified Data.Map as Map
```

Creare una mappa è semplice. Creiamone una con alcuni linguaggi di programmazione e i loro paradigmi:
```Haskell
let languages = Map.fromList [("Haskell", "Funzionale"), ("Python", "Imperativo"), ("Prolog", "Logico")]
```

E ora, che ne dite di ottenere il paradigma di Haskell?
```Haskell
Map.lookup "Haskell" languages
-- output: Just "Funzionale"
```

Aggiungere un nuovo linguaggio è facile:
```Haskell
let languagesUpdated = Map.insert "Rust" "Sistemi" languages
```

E se volessimo elencare tutti i linguaggi? Usa `Map.keys`:
```Haskell
Map.keys languagesUpdated
-- output: ["Haskell", "Python", "Prolog", "Rust"]
```

Per elencare i paradigmi, usa `Map.elems`:
```Haskell
Map.elems languagesUpdated
-- output: ["Funzionale", "Imperativo", "Logico", "Sistemi"]
```

Queste operazioni di base dovrebbero coprire la maggior parte degli usi, ma c'è molto altro da esplorare in `Data.Map`!

## Approfondimento

Il modulo `Data.Map` nella libreria standard di Haskell è costruito su alberi binari bilanciati, specificamente alberi AVL. Questa scelta garantisce che la maggior parte delle operazioni sulla mappa, come l'inserimento, l'eliminazione e la ricerca, possano essere effettuate in tempo O(log n), dove n è il numero di elementi nella mappa. È una scelta efficiente per molti casi d'uso, anche se non sempre la più veloce per tutti gli scenari.

C'è anche una sfumatura storica: prima che `Data.Map` diventasse la scelta prediletta, i programmatori Haskell spesso usavano liste di coppie per simulare gli array associativi. Tuttavia, le operazioni su tali strutture sono O(n) per la ricerca, rendendo `Data.Map` un notevole miglioramento in termini di prestazioni.

Ora, nonostante l'efficienza e l'utilità di `Data.Map`, non è sempre lo strumento migliore per ogni lavoro. Per compiti estremamente sensibili alle prestazioni, dove anche i tempi di ricerca O(log n) sono troppo lenti, o dove le chiavi sono sempre valori interi, gli array o le tabelle hash (tramite `Data.HashMap`) potrebbero offrire prestazioni migliori con tempi di accesso O(1).

L'ecosistema Haskell consente una varietà di strutture dati per soddisfare diverse esigenze, e `Data.Map` è una scelta eccellente per gli array associativi di uso generale, bilanciando facilità d'uso, flessibilità e prestazioni.
