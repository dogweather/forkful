---
title:                "Generazione di numeri casuali"
aliases:
- it/clojure/generating-random-numbers.md
date:                  2024-01-27T20:33:05.429183-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generazione di numeri casuali"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Generare numeri casuali nella programmazione riguarda la creazione di valori che non possono essere previsti logicamente in anticipo. I programmatori fanno ciò per vari motivi, inclusa la generazione di identificatori unici, la simulazione di scenari nello sviluppo di giochi o la selezione di campioni casuali dai dati per l'analisi.

## Come fare:

In Clojure, la generazione di numeri casuali è semplice e ci sono un paio di funzioni incorporate che possono essere utilizzate immediatamente.

Per generare un numero a virgola mobile casuale tra 0 (incluso) e 1 (escluso), puoi usare la funzione `rand`:

```Clojure
(rand)
;; Esempio di output: 0.7094245047062917
```

Se hai bisogno di un intero all'interno di un intervallo specifico, usa `rand-int`:

```Clojure
(rand-int 10)
;; Esempio di output: 7
```

Questo ti fornisce un intero casuale tra 0 (incluso) e il numero che passi come argomento (escluso).

Per generare un numero casuale all'interno di un intervallo specifico (non limitato agli interi), puoi combinare `rand` con l'aritmetica:

```Clojure
(defn rand-range [min max]
  (+ min (* (rand) (- max min))))
;; Uso
(rand-range 10 20)
;; Esempio di output: 14.857457734992847
```

Questa funzione `rand-range` restituirà un numero a virgola mobile casuale tra i valori `min` e `max` che specifichi.

Per scenari che richiedono distribuzioni più complesse o sequenze di numeri casuali dove è necessaria la ripetibilità (usando i semi), potrebbe essere necessario esplorare libreria aggiuntive che vanno oltre quanto incorporato.

## Approfondimento

Il meccanismo sottostante per la generazione di numeri casuali nella maggior parte dei linguaggi di programmazione, incluso Clojure, si basa tipicamente su un generatore di numeri pseudo-casuali (PRNG). Un PRNG utilizza un algoritmo per produrre una sequenza di numeri che approssima le proprietà dei numeri casuali. È importante notare che, poiché questi sono generati algoritmicamente, non sono veramente casuali, ma possono essere sufficienti per la maggior parte degli scopi pratici.

Nei primi giorni dell'informatica, generare numeri casuali di alta qualità era una sfida significativa, portando allo sviluppo di vari algoritmi per migliorare la casualità e la distribuzione. Per Clojure, le funzioni incorporate, come `rand` e `rand-int`, sono comode per l'uso quotidiano e coprono un ampio spettro di casi d'uso comuni.

Tuttavia, per applicazioni che richiedono sicurezza crittografica o metodi di campionamento statistico più complessi, gli sviluppatori di Clojure spesso si rivolgono a librerie esterne che offrono PRNG più robusti e specializzati. Librerie come `clj-random` forniscono accesso a una varietà più ampia di algoritmi e maggiore controllo sul seeding, il che può essere cruciale per simulazioni, applicazioni crittografiche o qualsiasi dominio in cui la qualità e la prevedibilità della sequenza di numeri casuali potrebbero avere implicazioni significative.

Sebbene le capacità incorporate di Clojure per generare numeri casuali siano adeguate per molti compiti, esplorare librerie esterne può offrire approfondimenti più profondi e opzioni per applicazioni su misura o più critiche.
