---
title:                "Interpolare una stringa"
html_title:           "Elixir: Interpolare una stringa"
simple_title:         "Interpolare una stringa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Interpolazione di una stringa è quando un programma sostituisce parti di una stringa con valori dinamici durante l'esecuzione. I programmatori lo fanno per rendere le stringhe più dinamiche e ridurre la redundanza nel codice.

## Come fare:
Un esempio di interpolazione stringa in Elixir è il seguente:

```Elixir
nome = "Giulia"
"\nCiao #{nome}, benvenuto!"
```

Questo codice produrrà:

```Elixir
"Ciao Giulia, benvenuto!"
```

Effettivamente, il programma ha sostituito la variabile ```nome``` con il suo valore durante l'esecuzione.

Un altro modo per interpolare una stringa è utilizzando la funzione ```String.interpolate/1```. Vediamo un esempio:

```Elixir
defmodule Messaggio do
  def hello(name) do
    String.interpolate("Ciao #{name}, benvenuto!")
  end
end

Messaggio.hello("Giulia")
```

Questo produrrà lo stesso output del primo esempio.

## Approfondimento:
Interpolazione di stringa è diventata un metodo popolare tra i programmatori per rendere il codice più pulito e leggibile. In passato, gli sviluppatori utilizzavano concatenazione di stringhe per ottenere lo stesso risultato ma questo rallentava notevolmente il processo di scrittura del codice. Inoltre, in Elixir è possibile utilizzare anche la funzione ```String.replace/3``` per ottenere lo stesso risultato, tuttavia la funzione ```String.interpolate/1``` semplifica notevolmente il processo.

## Vedi anche:
Per ulteriori informazioni sulla sintassi di interpolazione di stringa in Elixir, puoi consultare il sito ufficiale della documentazione: https://hexdocs.pm/elixir/Kernel.SpecialForms.html#%23%7B%7D/1