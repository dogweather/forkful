---
title:    "Haskell: Iniziare un nuovo progetto"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Perché

Stai pensando di iniziare un nuovo progetto in Haskell ma ti chiedi "perché dovrei farlo?". La risposta è semplice: Haskell è un linguaggio funzionale puro che ti permette di scrivere codice elegante, robusto e altamente scalabile. Inoltre, è particolarmente adatto per la gestione di complessi problemi matematici e algoritmi. 

## Come Iniziare

Per iniziare un nuovo progetto in Haskell, è necessario avere installato sul tuo computer il compilatore GHC (Glasgow Haskell Compiler) e un editor di testo. Una volta installati, puoi aprire il tuo editor di testo e seguire questi passaggi:

1. Definisci il tipo di dato che vuoi utilizzare:
```
Haskell data Person = Person { name :: String, age :: Int }
```

2. Crea una lista di persone:
```
Haskell people = [ Person "Mario" 24, Person "Giulia" 27, Person "Luigi" 30 ]
```

3. Filtra le persone in base all'età:
```
Haskell adults = filter (\person -> age person >= 18) people
```

4. Stampa i nomi delle persone adulte:
```
Haskell putStrLn $ "Nomi delle persone adulte: " ++ map name adults
```

## Approfondimenti

Per essere più efficace e produttivo nello sviluppo di progetti in Haskell, ecco alcune cose che dovresti prendere in considerazione:

- Usa le funzioni pure: evita gli effetti collaterali e il codice diventerà più facile da leggere, testare e mantenere.
- Sfrutta la tipizzazione statica: ti aiuterà a prevenire molti errori di runtime.
- Usa le funzioni ad altissimo ordine: permettono di scrivere codice modulare e conciso.
- Diventa familiare con le monadi: ti permettono di gestire il flusso dei dati in modo efficiente e sicuro.
- Sfrutta il pattern matching: ti aiuterà a scrivere codice più elegante e intuitivo.

## Vedi Anche

- [Introduzione a Haskell](https://wiki.haskell.org/Introduction)
- [GHC - Guida al compilatore Haskell](https://wiki.haskell.org/GHC)
- [Funzioni pure in Haskell](https://wiki.haskell.org/Pure_function)
- [Gestione degli effetti collaterali in Haskell](https://wiki.haskell.org/IO_Inside)
- [Tipizzazione statica in Haskell](https://wiki.haskell.org/Why_Haskell_matters)
- [Funzioni ad altissimo ordine in Haskell](https://wiki.haskell.org/Higher_order_function)
- [Monadi in Haskell](https://wiki.haskell.org/Monad)
- [Pattern matching in Haskell](https://wiki.haskell.org/Pattern_matching)