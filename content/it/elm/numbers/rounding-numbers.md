---
title:                "Arrotondamento dei numeri"
aliases:
- /it/elm/rounding-numbers/
date:                  2024-01-26T03:44:00.434283-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arrotondamento dei numeri"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/rounding-numbers.md"
---

{{< edit_this_page >}}

## Cos'è e il perché?

Arrotondare i numeri significa modificare un decimale al valore intero più vicino o ad un numero specificato di cifre decimali. I programmatori arrotondano per ridurre la complessità, migliorare la leggibilità o soddisfare i requisiti di precisione.

## Come fare:

Il modulo `Basics` di Elm fornisce funzioni utili per l'arrotondamento: `round`, `floor` e `ceiling`. Ecco come utilizzarle.

```elm
import Basics exposing (round, floor, ceiling)

-- Arrotonda al numero intero più vicino
round 3.14    --> 3
round 3.5     --> 4

-- Arrotonda verso il basso
floor 3.999   --> 3

-- Arrotonda verso l'alto
ceiling 3.001 --> 4

-- Tronca i decimali senza arrotondare
truncate 3.76 --> 3
```

Elm fornisce anche `toLocaleString` per l'arrotondamento a un numero fisso di posizioni decimali:

```elm
import Float exposing (toLocaleString)

-- Arrotonda a due cifre decimali
toLocaleString 2 3.14159 --> "3.14"
```

## Approfondimento

Elm è un linguaggio funzionale fortemente tipizzato che relega gli effetti collaterali ai "margini" dell'architettura. Questo significa che funzioni come l'arrotondamento devono essere pure e prevedibili. Storicamente, l'arrotondamento è un'operazione comune in molti linguaggi di programmazione che gestiscono l'imprecisione dell'aritmetica a virgola mobile.

L'approccio di Elm all'arrotondamento è diretto - le funzioni sono pure e aderiscono alle definizioni matematiche di round, floor e ceiling. Elm anticipa le esigenze comuni fornendo funzioni incorporate, dato che la gestione della precisione è un requisito frequente, soprattutto in finanza e grafica.

Alternative alle funzioni incorporate di Elm potrebbero includere implementazioni personalizzate usando operazioni aritmetiche, ma ciò aggiungerebbe complessità non necessaria quando la libreria standard fa già efficientemente il lavoro.

Nella versione attuale, Elm utilizza l'aritmetica a virgola mobile sottostante di JavaScript per queste operazioni, rimanendo quindi coerente con lo standard IEEE 754, il che è qualcosa da ricordare quando si considera la precisione e potenziali errori di virgola mobile.

## Vedi Anche

- Documentazione ufficiale del modulo `Basics` di Elm: https://package.elm-lang.org/packages/elm/core/latest/Basics
- Uno sguardo dettagliato su come funzionano i numeri in virgola mobile nel calcolo: https://floating-point-gui.de/
- Modulo `Float` di Elm per ulteriori operazioni con numeri a virgola mobile: https://package.elm-lang.org/packages/elm/core/latest/Float
