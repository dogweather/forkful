---
title:                "Elixir: Estrazione di sottostringhe"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Come programmatori, spesso abbiamo bisogno di manipolare le stringhe nei nostri programmi. Una delle operazioni più comuni che possiamo fare è l'estrazione di sottostringhe. Questa operazione ci consente di ottenere solo una parte della stringa originale. In questo articolo, esploreremo come possiamo fare ciò utilizzando il linguaggio di programmazione Elixir.

## Come fare

Per estrarre una sottostringa in Elixir, possiamo utilizzare la funzione `slice`. Questa funzione accetta tre argomenti: la stringa originale, l'indice di inizio e l'indice di fine. Ad esempio, se volessimo estrarre la sottostringa "world" dalla stringa "Hello world!", possiamo scrivere il seguente codice:

```Elixir
str = "Hello world!"
result = slice(str, 6, 10)
IO.puts(result) # output: "world"
```

Possiamo anche utilizzare indici negativi per contare dalla fine della stringa. Ad esempio, se volessimo estrarre la parte finale della stringa "Hello world!", possiamo farlo in questo modo:

```Elixir
str = "Hello world!"
result = slice(str, -5, -1)
IO.puts(result) # output: "world"
```

Inoltre, possiamo utilizzare l'operatore `..` per definire un intervallo di indici. Ad esempio, se volessimo estrarre i caratteri da 3 a 7 dalla stringa "Hello world!", possiamo farlo in questo modo:

```Elixir
str = "Hello world!"
result = slice(str, 3..7)
IO.puts(result) # output: "lo wo"
```

## Approfondimento

Oltre alla funzione `slice`, Elixir ci offre anche altre opzioni per estrarre substrati. Ad esempio, possiamo utilizzare la funzione `substring` che accetta due argomenti: la stringa originale e un intervallo di indici. Inoltre, possiamo utilizzare il modulo `String` e le sue funzioni `sub_string` e `substring/2` per estrarre sottostringhe in base a condizioni specifiche.

Per maggiori informazioni sulle diverse opzioni per estrarre sottostringhe in Elixir, si consiglia di consultare la documentazione ufficiale del linguaggio.

## Vedi anche

- [Documentazione ufficiale di Elixir](https://hexdocs.pm/elixir/String.html#slice/3)
- [Esercizi di pratica per l'estrazione di sottostringhe in Elixir](https://www.codewars.com/kata/search/slice%20elixir)
- [Tutorial su come utilizzare le funzioni per le stringhe in Elixir](https://blog.appsignal.com/2018/06/26/elixir-string-functions-you-need.html)