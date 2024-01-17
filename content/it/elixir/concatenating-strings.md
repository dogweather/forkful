---
title:                "Concatenazione di stringhe"
html_title:           "Elixir: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Cosa & perché?
La concatenazione di stringhe è il processo di unire più stringhe in una singola stringa. I programmatori spesso utilizzano questo metodo per creare output più complessi o per combinare diversi elementi di testo.

## Come fare:
Un modo semplice per concatenare stringhe in Elixir è utilizzando l'operatore `<>`, che unisce due stringhe in una sola. Ad esempio:

```Elixir
"Buon" <> "giorno" 
# Output: Buongiorno
```

È anche possibile utilizzare l'operatore `<>` per concatenare una stringa a un valore di un'altra variabile, come mostrato di seguito:

```Elixir
nome = "Maria"
"Ciao" <> nome
# Output: Ciao Maria
```

Se si desidera concatenare più di due stringhe, è possibile ripetere l'operatore `<>` più volte, come nell'esempio seguente:

```Elixir
"Hello" <> " " <> "world" <> "!"
# Output: Hello world!
```

## Approfondimento:
La concatenazione di stringhe è comune in molti linguaggi di programmazione, poiché è un modo semplice e veloce per unire diverse parti di testo. Tuttavia, una possibile alternativa in Elixir è l'utilizzo di liste, che possono essere concatenate utilizzando l'operatore `++`. Ad esempio:

```Elixir
liste = ["Welcome", "to", "Elixir"]
"Hello" ++ " " ++ lista ++ "!"
# Output: Hello to Elixir!
```

Un'altra alternativa è l'utilizzo di la funzione `Enum.join`, che unisce una lista di stringhe utilizzando un delimitatore:

```Elixir
liste = ["one", "two", "three"]
Enum.join(liste, "-")
# Output: one-two-three
```

Per quanto riguarda l'implementazione, Elixir offre una funzione nativa `Kernel.<>` per la concatenazione di stringhe, che alla fine viene convertita in binari per ottimizzare le prestazioni.

## Vedere anche:
- [Guida Elixir sulle stringhe](https://elixir-lang.org/getting-started/string.html)
- [Documentazione ufficiale Elixir](https://hexdocs.pm/elixir/String.html#concatenation/2)