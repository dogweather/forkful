---
title:                "Elixir: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui si potrebbe voler convertire una stringa in minuscolo in Elixir. Forse stai cercando di rendere uniforme il testo inserito dagli utenti nel tuo programma, o forse stai eseguendo una ricerca di stringhe in modo case-insensitive. Qualunque sia il motivo, la conversione di una stringa in minuscolo è un'operazione fondamentale da conoscere in Elixir.

## Come

Per convertire una stringa in minuscolo in Elixir, possiamo utilizzare la funzione `String.downcase/1`. Prende una stringa come input e restituisce una nuova stringa con tutti i caratteri convertiti in minuscolo. Ecco un esempio di come utilizzarlo:

```Elixir
iex> String.downcase("Ciao Mondo")
"ciao mondo"
```

Come puoi vedere, la stringa "Ciao Mondo" viene convertita in "ciao mondo". È importante notare che la funzione `String.downcase/1` non modifica la stringa originale, ma ne restituisce una nuova.

Possiamo anche utilizzare il modulo `String` per convertire una lista di stringhe in minuscolo, utilizzando la funzione `String.downcase/2`. Prende una lista di stringhe come primo argomento e un atomo come secondo argomento per specificare la lingua in cui dovrebbe essere eseguita la conversione. Ad esempio:

```Elixir
iex> String.downcase(["Ciao", "Mondo"], :it)
["ciao", "mondo"]
```

Questo converte le stringhe in minuscolo secondo la lingua italiana.

## Deep Dive

Se vuoi esplorare ulteriormente come Elixir gestisce la conversione di stringhe in minuscolo, puoi dare uno sguardo al codice sorgente della funzione `String.downcase/1`. Puoi farlo utilizzando il comando `h` nella tua console IEx e passando il nome della funzione come argomento:

```Elixir
iex> h String.downcase/1
```

Questo ti mostrerà il codice sorgente della funzione e ti aiuterà a capire meglio come funziona.

Inoltre, è importante ricordare che la funzione `String.downcase/1` utilizza le regole di conversione della lingua specificata in `Locale`. Se la lingua non è specificata, viene utilizzata la lingua corrente del sistema. Puoi leggere di più sulla gestione dei linguaggi e delle localizzazioni in Elixir nella [documentazione ufficiale](https://hexdocs.pm/elixir/String.html#module-localization).

## Vedi Anche

- [String documentation](https://hexdocs.pm/elixir/String.html)
- [Locale documentation](https://hexdocs.pm/elixir/Locale.html)