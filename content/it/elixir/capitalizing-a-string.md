---
title:                "Capitalizzando una stringa"
html_title:           "Elixir: Capitalizzando una stringa"
simple_title:         "Capitalizzando una stringa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

La capitalizzazione di una stringa è il processo di convertire la prima lettera di ogni parola in maiuscolo, mentre le restanti lettere rimangono in minuscolo. I programmatori spesso lo fanno per renderizzare una stringa in maniera più leggibile o per formattarla in un modo specifico.

## Come fare:

```Elixir
stringa = "ciao mondo"
stringa_capitalizzata = String.capitalize(stringa)

IO.puts(stringa_capitalizzata)
# Output: Ciao mondo 
```

In questo esempio, utilizzando la funzione `String.capitalize(stringa)`, la variabile `stringa` viene modificata e viene stampata in console una nuova stringa capitalizzata.

## Approfondimento:

In passato, la capitalizzazione di una stringa era un processo complesso che richiedeva l'uso di diverse funzioni e operatori. Con Elixir, ora è possibile farlo in modo semplice grazie alla funzione `String.capitalize`.

Un'alternativa alla capitalizzazione di una stringa è l'utilizzo del modulo `String`, che fornisce una varietà di funzioni utili per la manipolazione di stringhe.

Per implementare la funzione `String.capitalize`, Elixir utilizza l'algoritmo di trasformazione UTF-8, che converte automaticamente ogni carattere in maiuscolo o minuscolo in modo appropriato.

## Vedi anche:

- [Documentazione Elixir su String.capitalize](https://hexdocs.pm/elixir/String.html#capitalize/1)
- [Guida introduttiva su Elixir](https://elixir-lang.org/getting-started/introduction.html)
- [Articolo su Elixir linguaggio funzionale](https://www.tecnologie.net/eilixir-il-linguaggio-funzionale-che-sta-rivoluzionando-lo-sviluppo/)