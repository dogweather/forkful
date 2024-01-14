---
title:    "Elixir: Convertire una stringa in minuscolo"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Perché

Convertingire una stringa in minuscolo è un'operazione fondamentale in programmazione, specialmente quando si lavora con dati di input da utenti o database. Inoltre, rende più facile la manipolazione delle stringhe e l'effettuazione di confronti di testo.

## Come fare

Per convertire una stringa in minuscolo in Elixir, possiamo utilizzare la funzione `String.downcase/1`. Prende come argomento una stringa e restituisce una nuova stringa in cui tutti i caratteri sono convertiti in minuscolo. Vediamo un esempio di come questa funzione funziona in pratica:

```Elixir
iex> String.downcase("ELIXIR")
"elixir"
```

Possiamo anche utilizzare l'operatore `|>` per facilitare la lettura del codice, come mostrato di seguito:

```Elixir
iex> "ELIXIR" |> String.downcase
"elixir"
```

Inoltre, se vogliamo convertire una singola lettera in minuscolo, possiamo utilizzare la funzione `String.to_char_list/1` per convertire la stringa in una lista di caratteri, applicare la funzione `String.downcase/1` e poi convertire la lista di nuovo in una stringa con `List.to_string/1`:

```Elixir
iex> "E" |> String.to_char_list |> String.downcase |> List.to_string
"e"
```

## Approfondimento

È importante notare che la funzione `String.downcase/1` utilizza le regole del locale per determinare come convertire la stringa in minuscolo. Il locale è una configurazione che indica le impostazioni di notazione e alfabetizzazione di un determinato paese o regione. Questo significa che la conversione di una stringa in minuscolo in un locale diverso può produrre un risultato diverso.

È anche possibile utilizzare la funzione `String.downcase/2` per specificare manualmente una lingua quando si converte una stringa in minuscolo. Ad esempio, se vogliamo convertire una stringa in minuscolo utilizzando le regole inglesi, possiamo farlo nel seguente modo:

```Elixir
iex> String.downcase("ÉLIXIR", :en)
"élixir"
```

Inoltre, è possibile utilizzare l'operatore `<>` per concatenare stringhe e più funzioni `String.downcase/2` per applicare la conversione in modo selettivo a determinate parti di una stringa. Ad esempio:

```Elixir
iex> "This is an EXAMPLE" |> String.downcase(:en) <> "string"
"this is an exampleSTRING"
```

## Vedi anche

- [Elixir String Module](https://hexdocs.pm/elixir/String.html)
- [Understanding String Casing in Elixir](https://medium.com/@phanindramoganti/understanding-string-casing-in-elixir-87756c727f2e) (in inglese)