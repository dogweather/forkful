---
title:                "Elixir: Convertire una stringa in minuscolo"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte situazioni in cui potresti avere bisogno di convertire una stringa in lettere minuscole durante la programmazione in Elixir. Ad esempio, potresti dover confrontare due stringhe senza tener conto delle maiuscole e minuscole, o forse stai lavorando con dati inseriti dall'utente e vuoi essere sicuro che la stringa sia uniformemente formattata. In entrambi i casi, la conversione di una stringa in lettere minuscole può essere molto utile.

## Come Fare

Per convertire una stringa in lettere minuscole in Elixir, possiamo utilizzare la funzione `String.downcase/1` o `String.downcase/2`. La prima funzione prende in input una stringa e restituisce una nuova stringa con tutti i caratteri convertiti in minuscolo. La seconda funzione accetta un'opzione aggiuntiva per specificare la lingua da utilizzare per la conversione.

```elixir
iex> String.downcase("Ciao Mondo")
"ciao mondo"

iex> String.downcase("Привет мир", :cyrillic)
"привет мир"
```

Possiamo anche usare la funzione `String.downcase!/1` se vogliamo modificare direttamente la stringa originale invece di crearne una nuova.

```elixir
iex> string = "HELLO WORLD"
iex> String.downcase!(string)
"hello world"
iex> string
"hello world"
```

È importante notare che la conversione in minuscolo dipende dalla codifica dei caratteri utilizzata. Se la stringa contiene caratteri con tono o segni diacritici, potrebbe essere necessario utilizzare la funzione `String.downcase/2` con l'opzione `:unicode` per un risultato corretto.

```elixir
iex> String.downcase("Qué tal?", :unicode)
"qué tal?"
```

## Approfondimento

La conversione di una stringa in lettere minuscole in Elixir si basa sulle regole della libreria di standard Unicode. Ciò significa che non solo le lettere della lingua inglese verranno convertite correttamente, ma anche quelle di molte altre lingue come il russo, il cinese e l'arabo.

Inoltre, la conversione in lettere minuscole è sensibile alla localizzazione, il che significa che le regole possono variare a seconda della lingua impostata nel sistema operativo. Questo è importante da considerare se il tuo codice deve essere eseguito su più sistemi con lingue diverse.

## Vedi Anche

- [Documentazione ufficiale di Elixir per String.downcase/1](https://hexdocs.pm/elixir/String.html#downcase/1)
- [Documentazione ufficiale di Elixir per String.downcase!/1](https://hexdocs.pm/elixir/String.html#downcase!/1)
- [Altri metodi per la manipolazione di stringhe in Elixir](https://elixirschool.com/en/lessons/basics/strings/)