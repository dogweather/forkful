---
title:                "Convertire una stringa in minuscolo."
html_title:           "Elixir: Convertire una stringa in minuscolo."
simple_title:         "Convertire una stringa in minuscolo."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

La conversione di una stringa in minuscolo è un'operazione comune nella programmazione, che può essere utile per confrontare stringhe senza differenziare tra lettere maiuscole e minuscole o per formattare l'input dell'utente in modo uniforme. In Elixir, esistono diverse opzioni per convertire una stringa in minuscolo, a seconda delle esigenze del tuo progetto.

## Come fare

```Elixir
stringa = "Converti in MINUSCOLO"
min_stringa = String.downcase(stringa)
IO.puts(min_stringa) # output: "converti in minuscolo"
```

In questo esempio, utilizziamo la funzione built-in `String.downcase/1` per convertire la stringa in minuscolo. Questo metodo è utile quando non è necessario modificare la stringa originale.

Un'altra opzione è utilizzare la funzione `String.to_lower/1` che restituisce una nuova stringa in minuscolo, senza modificare quella originale:

```Elixir
stringa = "CONVERTI in minuscolo"
lower_stringa = String.to_lower(stringa)
IO.puts(lower_stringa) # output: "converti in minuscolo"
IO.puts(stringa) #output: "CONVERTI in minuscolo"
```

Se invece vuoi trasformare l'input dell'utente in minuscolo prima di elaborarlo, puoi utilizzare la funzione `String.downcase/2` passando come secondo argomento l'alfabeto della lingua dell'utente. In questo modo, la conversione sarà conforme alle regole di quella lingua:

```Elixir
user_input = "CIAO A TuTTi!"
lower_input = String.downcase(user_input, :italian)
IO.puts(lower_input) # output: "ciao a tutti!"
```

## Approfondimento

Elixir fornisce anche la funzione `String.capitalize/1` per convertire una stringa in una versione con la prima lettera maiuscola e il resto in minuscolo:

```Elixir
nome = "mario"
cognome = "rossi"
stringa = nome <> " " <> cognome
lower_stringa = String.capitalize(stringa)
IO.puts(lower_stringa) # output: "Mario Rossi"
```

È importante ricordare che in Elixir le stringhe sono immutabili, quindi ogni operazione di modifica restituirà una nuova stringa anziché modificare quella originale. Per convertire una stringa in un dato punto separatore, si può utilizzare anche la funzione `String.split/2` combinata con `Enum.map/2`:

```Elixir
stringa = "Converti in MINUSCOLO"
words_list = String.split(stringa, " ")
lower_words = Enum.map(words_list, &String.downcase/1)
lower_stringa = Enum.join(lower_words, " ")
IO.puts(lower_stringa) # output: "converti in minuscolo"
```

## Vedi anche

- Documentazione ufficiale di Elixir sulle stringhe: https://hexdocs.pm/elixir/String.html
- Tutorial di base su Elixir: https://elixir-lang.org/getting-started/introduction.html
- Guida pratica all'utilizzo di Elixir: https://elixirschool.com/it/