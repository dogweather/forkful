---
title:                "Eliminazione di caratteri che corrispondono a un pattern"
date:                  2024-01-20T17:42:15.040690-07:00
model:                 gpt-4-1106-preview
simple_title:         "Eliminazione di caratteri che corrispondono a un pattern"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
Cancellare i caratteri che corrispondono a un pattern significa selezionare e rimuovere specifici caratteri da una stringa seguendo una regola definita. Programmatori fanno questo per pulire i dati, validare input, o manipolare il testo per requisiti specifici.

## How to:
Gleam non ha una libreria standard che supporti espressioni regolari nativamente. Si può usare funzioni come `string.replace` o librerie esterne. Ecco un esempio che usa `string.replace` per eliminare i numeri da una stringa:

```gleam
import gleam/string

fn strip_numbers(text: String) -> String {
  string.replace(text, "0", "")
  |> string.replace(_, "1", "")
  // Continua per tutti gli altri numeri...
  |> string.replace(_, "9", "")
}

fn main() {
  let result = strip_numbers("Il 7 e l'8 sono in mezzo al 6 e al 9.")
  assert result == "Il  e l' sono in mezzo al  e al ."
  result
}
```

Output:
```
"Il  e l' sono in mezzo al  e al ."
```

## Deep Dive
Gleam è un nuovo linguaggio di programmazione staticamente tipizzato per la BEAM, l'ambiente virtuale di linguaggi come Erlang ed Elixir. Da quando è uno più giovane nella famiglia BEAM, molte funzionalità come l'elaborazione avanzata di stringhe attraverso le regex non sono presenti nativamente come nei suoi 'fratelli maggiori'. I pattern possono quindi essere eliminati utilizzando funzioni di stringhe o manipolando la stringa a mano. Come alternativa si può ricorrere a librerie esterne, come `gleam_otp/regular_expressions`, che sfruttano le capacità di regex di Erlang. Da considerare, però, il trade-off tra prestazioni e leggibilità quando si utilizzano queste soluzioni.

## See Also
Per approfondire, ecco alcune risorse utili:
- `gleam_otp/regular_expressions` per regex support in Gleam: [https://hex.pm/packages/gleam_otp](https://hex.pm/packages/gleam_otp)