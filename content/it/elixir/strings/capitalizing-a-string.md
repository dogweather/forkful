---
title:                "Capitalizzare una stringa"
aliases:
- it/elixir/capitalizing-a-string.md
date:                  2024-02-03T19:04:52.850156-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizzare una stringa"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?

Capitalizzare una stringa significa convertire la prima lettera della stringa in maiuscolo, assicurandosi che le restanti lettere siano in minuscolo. Questa azione è comunemente necessaria per formattare l'input dell'utente o visualizzare testi nelle interfacce utente, dove la coerenza e la leggibilità sono importanti.

## Come fare:

Elixir offre un modo semplice per capitalizzare le stringhe utilizzando le sue funzioni integrate senza la necessità di librerie di terze parti. Ecco un esempio semplice:

```elixir
string = "elixir programming"
capitalized_string = String.capitalize(string)
IO.puts capitalized_string
```

Output:

```
Elixir programming
```

Per casi in cui è necessario un maggiore controllo o una logica di capitalizzazione più complessa, si potrebbero combinare diverse funzioni di String. Ad esempio, se si vuole capitalizzare ogni parola in una frase, si può dividere la frase in parole, capitalizzare ciascuna e poi unirle di nuovo:

```elixir
sentence = "elixir è divertente"
capitalized_sentence = sentence 
                        |> String.split() 
                        |> Enum.map(&String.capitalize/1) 
                        |> Enum.join(" ")

IO.puts capitalized_sentence
```

Output:

```
Elixir È Divertente
```

Sebbene la libreria standard di Elixir copra la maggior parte delle esigenze, per manipolazioni del testo più sfumate, inclusa la capitalizzazione avanzata delle stringhe, si potrebbe esplorare librerie di terze parti come Cldr per l'internazionalizzazione, che possono offrire comportamenti di capitalizzazione specifici per locale.
