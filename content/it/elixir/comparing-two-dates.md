---
title:                "Confronto tra due date"
html_title:           "Elixir: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Che cosa & perché?

Comparare due date è l'atto di confrontare due oggetti di tipo data e determinare se sono uguali, precedenti o successive l'una all'altra. I programmatori spesso eseguono questo tipo di confronto per gestire correttamente le date all'interno dei loro programmi, ad esempio per ordinare eventi in ordine cronologico o per verificare la validità di una data inserita dall'utente.

## Come fare:

```Elixir
defmodule DateComparison do
  def compare(date1, date2) do
    if nafts.date_lt(date2, date1) do
      "La data #{date2} è precedente alla data #{date1}"
    elsif nafts.date_gt(date2, date1) do
      "La data #{date2} è successiva alla data #{date1}"
    else
      "Le due date sono uguali"
    end
  end
end

DateComparison.compare(~D[2020-01-01], ~D[2020-01-02])
# Output: "La data 2020-01-02 è successiva alla data 2020-01-01"

DateComparison.compare(~D[2020-01-15], ~D[2020-01-15])
# Output: "Le due date sono uguali"

DateComparison.compare(~D[2020-01-30], ~D[2020-01-20])
# Output: "La data 2020-01-20 è precedente alla data 2020-01-30"
```

## Approfondimento:

Comparare date è una funzione comunemente utilizzata in programmazione e, in realtà, è una delle operazioni più semplici che possiamo fare con le date. Tuttavia, alcuni linguaggi di programmazione potrebbero richiedere la conversione delle date in numeri interi prima di effettuare un confronto, mentre in Elixir è possibile confrontarle direttamente grazie al modulo `Nafts` del pacchetto `Elixir Date`.

In alternativa alla comparazione di date, alcune persone preferiscono trasformarle in oggetti di tipo `DateTime` e utilizzare metodi per confrontarli, come `DateTime.compare/2`.

## Vedi anche:

- Documentazione sul modulo `Nafts` di Elixir Date: https://hexdocs.pm/elixir/Naft.html
- Informazioni su Date e DateTime in Elixir: https://elixirschool.com/it/lessons/basics/date-and-time/
- Approfondimenti sulle funzioni di comparazione in Elixir: https://gist.github.com/rtfeldman/be7ff0535f8126128ace