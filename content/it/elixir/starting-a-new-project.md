---
title:                "Elixir: Avviare un nuovo progetto"
programming_language: "Elixir"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui qualcuno potrebbe voler iniziare un nuovo progetto in Elixir. Forse stai cercando un linguaggio di programmazione funzionale, o magari stai cercando una soluzione scalabile per il tuo progetto. Qualunque sia la ragione, Elixir offre una combinazione unica di funzionalità che lo rende un'ottima scelta per iniziare un nuovo progetto.

## Come fare

La prima cosa da fare è assicurarsi di avere installato Elixir sul tuo computer. Puoi seguire le istruzioni di installazione ufficiali sul sito web di Elixir. Una volta terminata l'installazione, puoi iniziare ad esplorare la sintassi di Elixir e imparare le sue funzionalità chiave.

Ecco un semplice esempio in cui definiamo una funzione che calcola la somma di due numeri:

```Elixir
defmodule Calcolatrice do
  def somma(a, b) do
    a + b
  end
end

IO.puts Calcolatrice.somma(2, 3)
```

L'output di questo codice sarà "5", poiché abbiamo chiamato la funzione somma con i parametri 2 e 3. Puoi utilizzare questo esempio come punto di partenza per creare il tuo progetto in Elixir.

## Approfondimento

Oltre alla sintassi intuitiva e alla scalabilità di Elixir, c'è un'altra caratteristica che lo rende un'ottima scelta per iniziare un nuovo progetto: l'Erlang Virtual Machine (VM). Elixir è costruito su di essa e questo si traduce in una stabilità e una tolleranza agli errori incredibili.

Inoltre, Elixir offre anche il concetto di supervisione. Ciò significa che puoi definire all'interno del tuo progetto dei processi che si occupano del monitoraggio e della gestione degli errori dei processi principali. Questo è particolarmente utile in ambienti distribuiti, in cui un singolo errore potrebbe avere conseguenze catastrofiche.

## Vedi anche

- [Documentazione ufficiale di Elixir](https://elixir-lang.org/docs.html)
- [Libreria di Elixir per la gestione di database](https://github.com/elixir-ecto/ecto)
- [Comunità italiana di Elixir](https://elixir-it.org/)