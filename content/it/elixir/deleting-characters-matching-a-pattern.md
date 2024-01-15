---
title:                "Eliminazione di caratteri corrispondenti a un modello"
html_title:           "Elixir: Eliminazione di caratteri corrispondenti a un modello"
simple_title:         "Eliminazione di caratteri corrispondenti a un modello"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte situazioni in cui può essere utile eliminare dei caratteri che corrispondono ad un determinato pattern all'interno di una stringa. Ad esempio, può essere utile pulire un input utente o rimuovere caratteri indesiderati da un testo.

## Come Fare

Usando la funzione `String.replace/4` possiamo facilmente eliminare i caratteri che corrispondono ad un pattern all'interno di una stringa. Per esempio:

```Elixir
string = "Elixir è un linguaggio di programmazione funzionale"
regex = ~r/[aàeèiìoòuù]/
String.replace(string, regex, "")
```

L'output di questo codice sarà: "lxr ì n lngg d prgrmmzn fncznl". Come puoi vedere, tutti i caratteri vocali sono stati eliminati dalla stringa.

Puoi anche fornire una funzione come argomento per sostituire dinamicamente i caratteri che corrispondono al pattern. Ad esempio:

```Elixir
string = "Prova123"
regex = ~r/\d/
String.replace(string, regex, fn _ -> "X" end)
```

L'output sarà: "ProvaXXX". In questo caso, la funzione sostituisce tutti i numeri con la lettera "X".

## Approfondimento

La funzione `String.replace/4` accetta un quarto argomento opzionale chiamato `count`, che specifica il numero massimo di sostituzioni da effettuare. Prendiamo ad esempio il seguente codice:

```Elixir
string = "Ciao mondo!"
regex = ~r/,/
String.replace(string, regex, "", 2)
```

L'output sarà "Ciao mondo". In questo caso, abbiamo specificato un numero massimo di due sostituzioni, quindi solo le prime due virgole saranno eliminate dalla stringa.

## Vedi Anche

- [Documentazione Elixir: String.replace/4](https://hexdocs.pm/elixir/String.html#replace/4)
- [RegExr: Online Regular Expression Tester](https://regexr.com/)