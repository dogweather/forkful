---
title:                "Generare numeri casuali"
html_title:           "Elixir: Generare numeri casuali"
simple_title:         "Generare numeri casuali"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché
In questo articolo parleremo di come generare numeri casuali utilizzando Elixir, l'ultima versione del linguaggio di programmazione funzionale. Generare numeri casuali può essere utile in molte situazioni, come ad esempio nel testing di algoritmi o nel creare giochi.

## Come
Utilizzando la funzione `:rand.uniform/1` possiamo generare numeri casuali compresi tra 0 e 1. Se vogliamo un intervallo diverso, possiamo specificarlo come parametro. Ad esempio, se volessimo generare numeri casuali tra 1 e 10, possiamo utilizzare la seguente sintassi:
```Elixir
:rand.uniform(10) + 1
```
Questo restituirà un numero compreso tra 1 e 10 inclusi.

Se invece vogliamo generare numeri casuali interi, possiamo utilizzare la funzione `:rand.uniform/2` specificando l'intervallo intero desiderato. Ad esempio, per generare un numero casuale tra 20 e 30, possiamo utilizzare:
```Elixir
:rand.uniform(20, 30)
```

Inoltre, è possibile generare numeri casuali basati su una distribuzione normale utilizzando la funzione `:rand.normal/0` o `:rand.normal/1` specificando la media e la deviazione standard. 

## Deep Dive
Elixir utilizza l'algoritmo Mersenne Twister per generare numeri casuali, che è noto per essere rapido e di alta qualità. Inoltre, con il modulo `:rand`, è possibile impostare un seed per generare una sequenza di numeri casuali ripetibile.

È importante notare che i numeri casuali in Elixir non sono veramente casuali in quanto basati su un algoritmo matematico. Tuttavia, è sufficiente per molte applicazioni e fornisce una buona approssimazione della casualità.

## Vedi anche
Elixir offre molte altre funzioni per generare numeri casuali e manipolare le sequenze. Per saperne di più, puoi consultare la documentazione ufficiale dellla libreria `:rand` oppure esplorare altre risorse online come i tutorial e i forum per gli utenti di Elixir.

- Documentazione ufficiale di Elixir sul modulo `:rand`: https://hexdocs.pm/elixir/Random.html
- Tutorial su come generare numeri casuali in Elixir: https://pragmaticstudio.com/courses/elixir/random-numbers
- Forum della community su Elixir: https://elixirforum.com/