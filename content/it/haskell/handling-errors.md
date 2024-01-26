---
title:                "Gestione degli errori"
date:                  2024-01-26T00:54:10.457494-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gestione degli errori"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/handling-errors.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
La gestione degli errori nella programmazione riguarda la gestione dell'inaspettato — cose che possono andare storte. I programmatori lo fanno per assicurarsi che i loro programmi possano affrontare queste situazioni con grazia, senza bloccarsi o produrre risultati errati.

## Come fare:
Haskell gestisce in modo robusto gli errori attraverso tipi come `Maybe` e `Either`. Ecco uno sguardo rapido:

```Haskell
safeDivide :: Integral a => a -> a -> Maybe a
safeDivide _ 0 = Nothing  -- Dividere per zero non va bene, quindi restituiamo Nothing.
safeDivide x y = Just (x `div` y)  -- Altrimenti, tutto ok, restituiamo il risultato in un Just.

-- Vediamolo in azione:
example1 :: Maybe Int
example1 = safeDivide 10 2  -- Just 5

example2 :: Maybe Int
example2 = safeDivide 10 0  -- Nothing
```

Per una gestione degli errori più complessa, entra in gioco `Either`:

```Haskell
safeDivideEither :: Integral a => a -> a -> Either String a
safeDivideEither _ 0 = Left "Errore di divisione per zero."  -- Questa volta, l'errore porta un messaggio.
safeDivideEither x y = Right (x `div` y)

-- E nell'uso:
example3 :: Either String Int
example3 = safeDivideEither 10 2  -- Right 5

example4 :: Either String Int
example4 = safeDivideEither 10 0  -- Left "Errore di divisione per zero."
```

## Approfondimento
Nel mondo di Haskell, la gestione degli errori ha una lunga storia. Ai tempi, gli errori potevano far crollare l'intero programma — niente divertimento. Il sistema di tipi di Haskell offre modi per rendere tutto ciò molto meno probabile. Abbiamo `Maybe` e `Either`, ma ci sono anche altri come `Exceptions` e `IO` per scenari diversi.

`Maybe` è semplice: ottieni `Just` qualcosa se va tutto bene, o `Nothing` se non è così. `Either` alza il livello consentendo di restituire un messaggio di errore (`Left`) o un risultato di successo (`Right`).

Entrambi sono puri, nel senso che non interferiscono con il mondo esterno — un grande affare in Haskell. Evitiamo le insidie delle eccezioni non controllate che affliggono alcuni altri linguaggi.

Per coloro che non si accontentano di `Maybe` e `Either`, librerie come `Control.Exception` forniscono una gestione degli errori più tradizionale, in stile imperativo, attraverso le eccezioni. Ma usarle troppo liberamente può complicare le cose, quindi la comunità spesso si attiene ai tipi.

## Vedi Anche
Approfondisci con:

- I documenti di Haskell: [Haskell](https://haskell.org/documentation)
- Ottimo per principianti: ["Impara Haskell per il tuo bene!"](http://learnyouahaskell.com/)