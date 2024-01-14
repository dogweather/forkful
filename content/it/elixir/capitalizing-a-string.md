---
title:                "Elixir: Maiuscolare una stringa"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Perché 

C'è un motivo molto semplice per cui dovresti imparare a capitalizzare una stringa in Elixir - rende il tuo codice più leggibile e coerente. Inoltre, può essere utile quando si lavora con dati provenienti da fonti esterne che potrebbero essere in formati diversi.

# Come fare 

Ecco un esempio su come capitalizzare una stringa utilizzando la funzione `String.capitalize/1` in Elixir:

```Elixir
iex> String.capitalize("ciao")
"Ciao"
```

Noterai che la lettera "c" è stata convertita in maiuscolo.

E se volessimo capitalizzare solo la prima lettera di ogni parola in una frase? Possiamo utilizzare la funzione `String.capitalize/2` specificando anche il flag `:words`:

```Elixir
iex> String.capitalize("ciao mondo", :words)
"Ciao Mondo"
```

Puoi anche creare la tua funzione per capitalizzare una stringa, ad esempio:

```Elixir
def capitalize_first_letter(str) do
  String.capitalize(String.at(str, 0)) <> String.slice(str, 1..-1)
end

iex> capitalize_first_letter("ciao")
"Ciao"
```

Questo codice prende la prima lettera della stringa, la converte in maiuscolo e la concatena con il resto della stringa.

# Approfondimento 

Ci sono alcune cose da tenere a mente quando si lavora con la funzione `String.capitalize/1`. Innanzitutto, la lettera "ß" (es-zet in tedesco) viene trattata in modo diverso rispetto alle altre lettere maiuscole. Inoltre, le lettere accentate e speciali devono essere gestite in modo appropriato al fine di ottenere i risultati desiderati.

Un altro aspetto da considerare è che la funzione `String.capitalize/2` è sensibile alle impostazioni di localizzazione. Questo significa che il risultato può variare a seconda della lingua correntemente utilizzata dall'ambiente Elixir.

# Vedi anche 

- [Documentazione su String.capitalize/1](https://hexdocs.pm/elixir/String.html#capitalize/1)
- [Documentazione su String.capitalize/2](https://hexdocs.pm/elixir/String.html#capitalize/2)
- [Funzionamento delle impostazioni di localizzazione in Elixir](https://hexdocs.pm/elixir/Kernel.html#c:0-get_locale/0)