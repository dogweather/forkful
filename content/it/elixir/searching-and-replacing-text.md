---
title:                "Elixir: Ricerca e sostituzione di testo"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Quando si programma in Elixir, uno dei problemi più comuni è la necessità di sostituire del testo all'interno di una stringa o di un file. Ecco perché è importante conoscere i diversi metodi per farlo in modo efficiente e accurato.

## Come

Elixir offre diversi modi per effettuare la ricerca e la sostituzione di testo. Il metodo più semplice è utilizzare la funzione `String.replace/3` che accetta tre argomenti: la stringa di origine, l'espressione da cercare e la stringa di sostituzione desiderata. Ad esempio:

```Elixir
String.replace("Ciao mondo", "Ciao", "Salve") # Output: Salve mondo
```

È possibile anche utilizzare le espressioni regolari per cercare e sostituire testo in modo più avanzato. Con l'operatore `=~` è possibile verificare se una stringa corrisponde a un'espressione regolare e, utilizzando l'operatore `<<<`, è possibile sostituire il testo corrispondente. Ad esempio:

```Elixir
"Ciao mondo" =~ ~r/Ciao/ # Output: true

"Ciao mondo" <<< ~r/Ciao/ # Output: Salve mondo
```

## Deep Dive

Se si vuole effettuare la ricerca e la sostituzione su un file di testo, è possibile utilizzare la libreria `File` di Elixir. Utilizzando le funzioni `File.read/1` e `File.write/2`, è possibile leggere il contenuto del file, effettuare le modifiche desiderate e sovrascrivere il file originale con il nuovo contenuto modificato. Ad esempio:

```Elixir
text = File.read("file.txt")

new_text = String.replace(text, "Ciao", "Salve")

File.write("file.txt", new_text)
```

Inoltre, Elixir offre anche la libreria `Stream` che permette di effettuare la ricerca e la sostituzione in modo "pigrizia", lavorando solo con i dati necessari e non caricando tutto in memoria.

## Vedi Anche

- [Documentazione ufficiale di Elixir sulla gestione delle stringhe] (https://hexdocs.pm/elixir/String.html)
- [Articolo su come utilizzare le espressioni regolari in Elixir] (https://elixirschool.com/it/lessons/basics/pattern-matching/)
- [Guida su come utilizzare la libreria `Stream` di Elixir] (https://blog.appsignal.com/2018/07/17/elixir-alchemy-the-power-of-piping-operators-with-streams.html)