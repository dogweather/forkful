---
title:                "Elixir: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché

Le espressioni regolari sono uno strumento potente per la manipolazione di testo e la ricerca di pattern all'interno di stringhe. Con Elixir, possono aumentare l'efficienza e la flessibilità della programmazione, aiutando a semplificare compiti come la validazione di input utente o l'estrazione di informazioni da grandi dataset.

## Come usarle

Per utilizzare le espressioni regolari in Elixir, è necessario importare il modulo Regex utilizzando `import Regex`. Dopo di che, è possibile utilizzare le funzioni fornite da questo modulo per creare ed eseguire le espressioni regolari.

Un esempio semplice sarebbe la verifica se una stringa contiene un numero di telefono valido seguendo il formato italiano. Utilizzando l'espressione regolare `~r/^\+39\d{10}$/`, possiamo verificare se una stringa inizia con "+39" e ha esattamente 10 numeri successivi. Di seguito un codice di esempio che utilizza questa espressione regolare:

```Elixir
import Regex

numero_telefono = "+393456789012"

if Regex.match?(~r/^\+39\d{10}$/, numero_telefono) do
  IO.puts "Numero di telefono valido"
else
  IO.puts "Numero di telefono non valido"
end

# Output:
# Numero di telefono valido
```

## Approfondimento

Le espressioni regolari offrono molte opzioni e syntax per creare e gestire pattern complessi, quindi è importante avere una buona comprensione di queste funzionalità per utilizzarle pienamente. Ad esempio, è possibile utilizzare i gruppi di cattura per estrarre parti specifiche di una stringa, o utilizzare le opzioni dei flag come "i" per rendere le espressioni regolari non case-sensitive.

Elixir offre anche alcune funzionalità uniche per le espressioni regolari, come la possibilità di utilizzarle all'interno delle funzioni `String.replace` e `String.split`. Questo permette di manipolare facilmente stringhe e modificarle in base ai pattern definiti.

## Vedi Anche
- [Documentazione sul modulo Regex](https://hexdocs.pm/elixir/Regex.html)
- [Tutorial sulla sintassi delle espressioni regolari](https://www.regular-expressions.info/elixir.html)
- [Elixir School: Espressioni Regolari](https://elixirschool.com/it/lessons/basics/regular-expressions/)