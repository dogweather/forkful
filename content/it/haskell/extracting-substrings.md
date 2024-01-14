---
title:                "Haskell: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Estrarre una sottostringa da una stringa più grande può sembrare un'operazione semplice, ma può essere molto utile per gestire e manipolare dati in modo più efficiente. Inoltre, l'estrazione di sottostringhe è un concetto fondamentale nella programmazione e nella manipolazione di stringhe, quindi è importante saperlo fare.

## Come fare

Estrarre una sottostringa è un'operazione che può essere eseguita in vari modi in Haskell. Vedremo alcuni esempi con il codice e il risultato di output.

```Haskell
-- Utilizzando la funzione take
take 5 "Ciao a tutti" -- Output: "Ciao "

-- Utilizzando la funzione drop
drop 5 "Ciao a tutti" -- Output: "a tutti"

-- Utilizzando la funzione takeWhile
takeWhile (/= ' ') "Ciao a tutti" -- Output: "Ciao"

-- Utilizzando la funzione dropWhile
dropWhile (/= 'l') "Ciao a tutti" -- Output: "l a tutti"

-- Utilizzando l'operatore di slicing !
"Ciao a tutti" ! 6 -- Output: "t"

-- Utilizzando la funzione substring del pacchetto text
T.take 5 "Ciao a tutti" -- Output: "Ciao "
```

Come possiamo vedere, ci sono diverse funzioni disponibili per estrarre sottostringhe in Haskell. La scelta dipenderà dalle nostre esigenze e dalla situazione in cui ci troviamo.

## Approfondimento

Oltre alle funzioni che abbiamo visto sopra, ci sono anche altre considerazioni da tenere in mente quando si lavora con le sottostringhe in Haskell.

Ad esempio, è importante prestare attenzione alla gestione delle eccezioni in caso di stringhe vuote o di indici non validi. Inoltre, è fondamentale comprendere il concetto di "slice inclusivo" e "slice esclusivo", ovvero se il carattere di arrivo è incluso o meno nella sottostringa estratta.

Inoltre, l'utilizzo di pacchetti come text o bytestring può portare a un'efficienza maggiore nell'estrazione di sottostringhe, soprattutto con stringhe molto grandi.

## Vedi anche

- [Documentazione di Haskell sulle stringhe](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-String.html)
- [Pacchetto text su Hackage](https://hackage.haskell.org/package/text)
- [Guida alla programmazione funzionale con Haskell](https://learnyouahaskell.com/chapters)