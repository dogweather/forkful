---
title:                "Arrotondamento dei numeri"
date:                  2024-01-26T03:44:53.884567-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arrotondamento dei numeri"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/rounding-numbers.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
Arrotondare i numeri consiste nel portare un valore al posto specificato più vicino - come da 2,56 a 3 se si arrotonda ai numeri interi. I programmatori lo fanno per semplificare o per soddisfare determinate specifiche numeriche, solitamente per evitare le sfumature causate da errori di precisione in virgola mobile o per rendere l'output più amichevole per l'utente.

## Come fare:
In Gleam, l'arrotondamento non è presente nella libreria standard al momento del mio ultimo controllo, ma ecco come si arrotonderebbe tipicamente un float al numero intero più vicino utilizzando direttamente le funzioni di Erlang:

```gleam
external fn erlang_round(Float) -> Int = "erlang" "round"

pub fn main() {
  let rounded = erlang_round(2.56)
  rounded // Risultato: 3
}
```

Risultato:
```
3
```

Hai in mente una precisione diversa? Diciamo, arrotondare a due cifre decimali? Abbiamo bisogno di un po' di matematica:

```gleam
pub fn round_to_two_places(num: Float) -> Float {
  let multiplier = 100.0
  let tmp = num * multiplier
  let round_tmp = erlang_round(tmp)
  round_tmp / multiplier
}

pub fn main() {
    round_to_two_places(2.569) // Risultato: 2.57
}
```

Risultato:
```
2.57
```

## Approfondimento
Storicamente, l'arrotondamento dei numeri è stato cruciale, specialmente nei calcoli finanziari e scientifici dove la precisione e gli standard contano moltissimo. Senza l'arrotondamento, si avrebbero lunghe sequenze decimali dappertutto, rendendo i calcoli impraticabili e inclini agli errori.

Nel mondo della programmazione, diversi linguaggi offrono approcci differenti, dalle funzioni integrate a librerie matematiche comprensive. L'arrotondamento può coinvolgere regole diverse - per esempio, "arrotondare per eccesso" (il metodo solito) o "arrotondare alla pari" (spesso usato nei calcoli finanziari per evitare pregiudizi).

Gleam, essendo un linguaggio giovane con radici in Erlang, si affida al robusto insieme di funzioni numeriche di Erlang. Man mano che il linguaggio cresce, potremmo vedere funzioni native introdotte, riducendo la necessità di chiamare routine esterne.

## Vedi anche
- Il modulo :math di Erlang per ulteriori calcoli numerici: https://erlang.org/doc/man/math.html
- Per uno sfondo su perché l'arrotondamento può diventare complicato, lo Standard IEEE per la Virgola Mobile: https://ieeexplore.ieee.org/document/8766229
- Interessato alla matematica dietro a questo? Controlla "Cosa ogni informatico dovrebbe sapere sull'aritmetica in virgola mobile": https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html
