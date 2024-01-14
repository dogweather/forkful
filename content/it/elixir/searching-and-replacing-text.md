---
title:    "Elixir: Ricerca e sostituzione di testo"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Il processo di ricerca e sostituzione del testo è un'operazione comune nella programmazione Elixir. Spesso, è necessario modificare grandi quantità di dati o semplicemente sostituire parti di una stringa in modo efficiente. Continua a leggere per scoprire come puoi farlo in modo semplice ed efficiente.

## Come Fare

La soluzione più comune per cercare e sostituire il testo in Elixir è utilizzare il metodo `String.replace/3`. Questo metodo prende tre argomenti: la stringa originale, il testo da cercare e la stringa di sostituzione. Ecco un esempio:

```Elixir
testo = "Ciao a tutti!"
String.replace(testo, "Ciao", "Salve")
# Output: "Salve a tutti!"
```

È anche possibile utilizzare espressioni regolari per rendere la ricerca e la sostituzione più flessibili. Per fare ciò, si può utilizzare il modulo `Regex` e il metodo `Regex.replace/3`. Ad esempio, se si desidera sostituire tutte le vocali di una stringa con la lettera "o", si può utilizzare il seguente codice:

```Elixir
testo = "Ciao a tutti!"
Regex.replace(~r/[aeiou]/, testo, "o")
# Output: "Coao o tutti!"
```

Ci sono anche altri metodi utili per la ricerca e sostituzione del testo, come ad esempio `String.replace_prefix/3` e `String.replace_suffix/3`, che permettono di specificare esattamente dove sostituire il testo all'interno della stringa originale. Si consiglia di dare un'occhiata alla documentazione di Elixir per ulteriori opzioni e metodi utili.

## Approfondimento

Il metodo `String.replace/3` è molto efficiente, ma può diventare un po' lento se si deve effettuare la sostituzione su una grande quantità di dati. In questo caso, si può utilizzare la libreria `Stream` per elaborare i dati in modo più efficiente, come ad esempio:

```Elixir
una_lista_di_testi = ["Ciao a tutti!", "Hello everyone!", "Bonjour à tous!"]
una_lista_di_testi
|> Stream.map(&String.replace(&1, "a", "o"))
|> Enum.to_list
# Output: ["Cioo o tutti!", "Hello everyone!", "Bonjouro touts!"]
```

In questo esempio, si sta utilizzando il metodo `Stream.map/2` per applicare la sostituzione a ogni elemento della lista e `Enum.to_list/1` per convertire il risultato in una lista.

Inoltre, è possibile accedere ai gruppi catturati in un'espressione regolare utilizzando il modificatore di sostituzione `$`. Ciò può risultare molto utile per sostituire solo parti specifiche di una stringa.

## Vedi Anche

- Documentazione ufficiale di Elixir sulle stringhe e le espressioni regolari: https://hexdocs.pm/elixir/String.html
- Tutorial su come usare le espressioni regolari in Elixir: https://elixirschool.com/it/lessons/basics/pattern-matching/
- Articolo sulle libreria `StringScanner`, che fornisce funzionalità avanzate per la ricerca e la sostituzione del testo: https://www.poeticoding.com/elixir/scanning-strings-in-elixir-with-the-stringscanner-module/