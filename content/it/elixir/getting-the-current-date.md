---
title:                "Ottenere la data corrente"
html_title:           "Java: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Che Cosa e Perché?

Ottenere la data corrente è l'atto di chiedere al tuo programma di darti la data di oggi, come è concepita dal computer. I programmatori lo fanno per ottenere un timestamp che può essere utilizzato per tracciare eventi o momenti specifici durante l'esecuzione del programma.

## Come Fare:

In Elixir, ottenere la data corrente è piuttosto semplice e si può fare in diversi modi. Ne vedremo due qui.

```elixir
# Metodo 1
data_corrente = Date.utc_today()
IO.puts(data_corrente)  

# Metodo 2
{:ok, data_corrente} = Date.new(Timex.today())
IO.puts(data_corrente)
```

## Approfondimento

Nella programmazione, le date e le ore sono gestite in molti modi diversi. Originariamente, Unix Time era la norma, che contava solo i secondi dal 1° gennaio 1970. Elixir, tuttavia, utilizza un approccio leggermente diverso. 

Lo standard di Elixir per gestire le date e le ore è il Calendar Sigil, che fornisce una serie di funzionalità per manipolare date, ore e fusi orari. 

Una delle principali differenze è che Elixir gestisce le date come strutture separate, piuttosto che come un singolo flusso di secondi. Questo può causare certe complicazioni quando si cerca di confrontare le date, ma una volta compreso come funzionano, si avrà un controllo molto maggiore su questa parte della programmazione. 

Un'alternativa valida alla gestione delle date è l'uso della libreria Timex, che fornisce molte funzioni per lavorare facilmente con date e ore.

## Ulteriori Letture
1. [Documentazione Elixir sul modulo Date](https://hexdocs.pm/elixir/Date.html)
2. [Documentazione Elixir sul modulo DateTime](https://hexdocs.pm/elixir/DateTime.html)
3. [Documentazione sulla Libreria Timex in Elixir](https://hexdocs.pm/timex/readme.html) 
4. [Approfondimento su Calendar Sigils](https://hexdocs.pm/elixir/master/Kernel.SpecialForms.html#sigil_D/2)