---
title:    "Elixir: Estrazione di sottostringhe"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Perché

L'estrazione delle sottostringhe è un'operazione fondamentale per molte applicazioni di programmazione, inclusa l'elaborazione dei dati e la manipolazione delle stringhe. In Elixir, esistono diverse funzioni e metodi predefiniti per consentire agli sviluppatori di estrarre facilmente specifiche parti di una stringa. Continuate a leggere per scoprire come fare!

## Come fare

Per estrarre una sottostringa in Elixir, possiamo utilizzare il metodo `String.slice/3`, che richiede tre argomenti: la stringa originale, l'indice di inizio e l'indice finale della sottostringa desiderata. Ad esempio, se vogliamo estrarre la sottostringa "car" dalla parola "caramella", possiamo usare il seguente codice:

```Elixir
stringa = "caramella"
String.slice(stringa, 2, 4)
```

L'output di questo codice sarà "car". Ricordate che gli indici in Elixir partono da 0, quindi il primo carattere ha indice 0, il secondo 1 e così via.

Possiamo anche utilizzare la funzione `String.slice/2` se vogliamo estrarre una sottostringa a partire da un certo indice fino alla fine della stringa. Ad esempio, se vogliamo estrarre la parte finale della parola "caramella" a partire dal terzo carattere, possiamo scrivere il seguente codice:

```Elixir
stringa = "caramella"
String.slice(stringa, 2)
```

L'output sarà "mella", poiché il terzo carattere ha indice 2 e la funzione `String.slice/2` restituirà la parte della stringa a partire da quell'indice fino alla fine.

## Approfondimenti

Oltre alle funzioni `String.slice/2` e `String.slice/3`, esistono altre funzioni utili per l'estrazione delle sottostringhe in Elixir. Ad esempio, possiamo utilizzare la funzione `String.split/3` per suddividere una stringa in una lista di sottostringhe in base a un carattere delimitatore specificato.

```Elixir
stringa = "caramella, cioccolato, gelato"
String.split(stringa, ", ")
```

L'output sarà la lista `["caramella", "cioccolato", "gelato"]`. Inoltre, possiamo usare la funzione `String.replace/4` per sostituire una sottostringa con un'altra in una stringa.

Per ulteriori informazioni sulle funzioni di estrazione delle sottostringhe in Elixir, vi consigliamo di consultare la documentazione ufficiale su [Stringhe](https://hexdocs.pm/elixir/String.html).

## Vedi anche

- [Pattern matching in Elixir](https://medium.com/@simonezurawski/pattern-matching-in-elixir-4a9e74a138f8)
- [Come utilizzare caratteri speciali in Elixir](https://www.learnelixir.dev/handlers/lesson2.html)
- [Come manipolare le stringhe in Elixir](https://socoded.com/blog/manipulating-strings-in-elixir/)