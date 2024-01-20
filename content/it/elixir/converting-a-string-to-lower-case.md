---
title:                "Convertire una stringa in minuscolo"
html_title:           "Arduino: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Conversione di stringhe in minuscolo in Elixir

## Che Cosa & Perché?

La conversione di una stringa in minuscolo è il processo di cambiamento di tutte le lettere maiuscole in una stringa in lettere minuscole. Gli sviluppatori lo fanno per garantire la coerenza dei dati, facilitare il confronto tra stringhe e per motivi di ordinamento.

## Come fare:

In Elixir, convertire una stringa in minuscolo è semplice grazie alla funzione `String.downcase/1`. Ecco un esempio:

```Elixir
stringa = "CIAO MONDO"
IO.puts String.downcase(stringa)
```

Output:
```Elixir
ciao mondo
```

Facile, vero?

## Approfondimento

Mentre l'operazione di conversione di una stringa in minuscolo può sembrare semplice, ci sono alcune cose interessanti che potresti non conoscere.

1. Contesto storico: la necessità di convertire le stringhe in minuscolo esiste fin dai primi giorni della programmazione. Ricordiamo quando le macchine erano gli emulatori di terminale, che non distinguevano tra maiuscole e minuscole.
2. Alternative: in Elixir, se preferisci non utilizzare la funzione `String.downcase/1`, puoi implementare la tua funzione per convertire una stringa in minuscolo. Ecco un esempio:

```Elixir
defmodule MyString do
  def to_downcase(s) do
    s
    |> String.to_charlist()
    |> Enum.map(fn c -> if c in ?A..?Z, do: c + 32, else: c end)
    |> List.to_string()
  end
end
```

3. Dettagli implementativi: La funzione `String.downcase/1` di Elixir è basata sulla libreria Unicode, quindi è in grado di gestire correttamente le stringhe multilingue.

## Vedi anche:

Se desideri ulteriori informazioni sulla manipolazione delle stringhe in Elixir, dai un'occhiata ai seguenti collegamenti:

- Documentazione ufficiale di Elixir su String: https://hexdocs.pm/elixir/String.html
- Articolo di blog su "Elixir Strings and Character Lists": https://learningelixir.joekain.com/elixir-strings-and-character-lists/
- Guida pratica su "Elixir String Functions": https://www.tutorialspoint.com/elixir/elixir_string_functions.htm

E remember, sempre alla ricerca della conoscenza!