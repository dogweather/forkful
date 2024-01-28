---
title:                "Generazione di numeri casuali"
date:                  2024-01-27T20:33:29.539919-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generazione di numeri casuali"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Generare numeri casuali nella programmazione può essere cruciale per creare simulazioni, testare, crittografia e giochi. In Gleam, è una funzionalità che consente agli sviluppatori di introdurre imprevedibilità o simulare scenari del mondo reale nelle loro applicazioni.

## Come fare:

Per generare numeri casuali in Gleam, si utilizza principalmente la libreria `gleam_random`. Questa libreria fornisce funzioni per generare interi casuali, float e altro. Prima di tutto, assicurati di aver aggiunto `gleam_random` al tuo file `rebar.config` o `mix.exs` come dipendenza.

Vediamo alcuni esempi:

### Generare un Intero Casuale

Per produrre un intero casuale all'interno di un intervallo specificato, puoi usare la funzione `int`:

```gleam
import gleam/random

pub fn generate_random_int() {
  let random_int = random.int(1, 10)
  random_int
}
```

Questa funzione genererà un intero casuale tra 1 e 10 inclusi.

### Generare un Float Casuale

Per ottenere un float casuale, usa la funzione `float`. Questo genera un float tra 0,0 e 1,0:

```gleam
import gleam/random

pub fn generate_random_float() {
  let random_float = random.float()
  random_float
}
```

### Esempio di Output

Eseguendo queste funzioni potresti ottenere output come:

- Per `generate_random_int()`: `5`
- Per `generate_random_float()`: `0.84372`

Ricorda, ogni esecuzione potrebbe portare a output diversi a causa della natura della casualità.

## Approfondimento

Il modulo `gleam_random` implementa un generatore di numeri pseudo-casuali (PRNG), il che significa essenzialmente che i numeri non sono veramente casuali ma sono difficili da prevedere, emulando la casualità. I PRNG operano partendo da un valore iniziale, noto come seme, e applicando operazioni matematiche per generare una sequenza di numeri.

Storicamente, i linguaggi e le librerie hanno implementato diversi algoritmi per i PRNG, come il Mersenne Twister o il Generatore Lineare Congruenziale (LCG). La scelta dell'algoritmo influisce sulla qualità della "casualità", con alcuni che sono più adatti per applicazioni crittografiche rispetto ad altri. Sebbene la libreria standard di Gleam offra comodità e facilità d'uso con il suo modulo `gleam_random`, potrebbe non essere sempre la scelta migliore per casi d'uso che richiedono casualità criptograficamente sicura. Per scopi crittografici, gli sviluppatori dovrebbero cercare librerie specificamente progettate per fornire generatori di numeri pseudo-casuali criptograficamente sicuri (CSPRNG), che sono progettati per resistere ad attacchi che potrebbero prevedere numeri futuri basandosi sull'osservazione di una sequenza di numeri generati.

In conclusione, mentre la funzionalità di generazione di numeri casuali di Gleam è robusta per le esigenze generali di programmazione, le applicazioni con requisiti di sicurezza specifici dovrebbero considerare soluzioni crittografiche dedicate per garantire l'integrità e la sicurezza della loro generazione di numeri casuali.
