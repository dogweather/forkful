---
title:                "Gestione degli errori"
aliases:
- it/elixir/handling-errors.md
date:                  2024-01-26T00:51:18.372831-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gestione degli errori"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/handling-errors.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Gestire gli errori significa scrivere codice che può affrontare situazioni impreviste. I programmatori lo fanno per prevenire crash e per assicurarsi che i loro programmi possano riprendersi elegantemente quando la legge di Murphy colpisce.

## Come fare:

In Elixir, si usa spesso il pattern matching e l'istruzione `case` per gestire diversi esiti, inclusi gli errori.

```elixir
defmodule Example do
  def divide(a, b) do
    case b do
      0 -> {:error, "Impossibile dividere per zero."}
      _ -> {:ok, a / b}
    end
  end
end

# Divisione riuscita
{:ok, result} = Example.divide(10, 2)
IO.puts("10 / 2 è #{result}")

# Tentativo di divisione per zero
{:error, reason} = Example.divide(10, 0)
IO.puts("Errore: #{reason}")
```

Output di esempio:
```
10 / 2 è 5.0
Errore: Impossibile dividere per zero.
```

Quando esegui questo codice Elixir, otterrai o una divisione riuscita o un messaggio di errore, a seconda del tuo input. Niente crash qui!

## Approfondimento

Tempo fa, la gestione degli errori era spesso legata al controllo dei valori di ritorno. Con le radici funzionali di Elixir, però, abbiamo il pattern matching e le tuple etichettate, come `{:ok, value}` o `{:error, reason}`, che sono più eleganti.

Ci sono altri modi per gestire gli errori in Elixir:

- **`try` e `rescue` di Elixir**, che assomigliano al tradizionale `try-catch` dei linguaggi imperativi ma sono usati meno frequentemente a causa della preferenza di Elixir per l'esplicità.
- **Supervisori e GenServers**, parte del framework OTP di Elixir, che riguardano più la tolleranza ai guasti. Sorvegliano il processo del tuo codice, pronti a riavviarlo se le cose vanno storte.

In termini di implementazione, Elixir si basa sulla robustezza di Erlang. Tratta gli errori come un altro tipo di messaggio da gestire con tutto il pattern matching e la bontà funzionale.

## Vedi Anche

Per ulteriori letture sulla gestione degli errori in Elixir, consulta:

- La guida ufficiale di Elixir sulla [gestione degli errori](https://elixir-lang.org/getting-started/try-catch-and-rescue.html).
- Scopri di più sui [processi e OTP](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html).
- Il forum di Elixir è sempre un buon posto per fare domande: [https://elixirforum.com](https://elixirforum.com).
