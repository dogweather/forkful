---
title:    "Elixir: Ricercare e sostituire testo"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Elixir, probabilmente ti sei imbattuto nella necessità di cercare e sostituire del testo in alcuni dei tuoi progetti. Potresti aver pensato che si tratta di un'operazione semplice, ma la verità è che ci sono alcuni trucchi e tecniche che possono semplificare il processo e renderlo più efficiente. In questo articolo, esploreremo il motivo per cui dovresti essere interessato a imparare a cercare e sostituire il testo in Elixir.

## Come fare

Per eseguire la ricerca e la sostituzione del testo in Elixir, puoi utilizzare la funzione `String.replace/4`. Questa funzione prende quattro argomenti: la stringa di input in cui cercare, il pattern da cercare, il pattern di sostituzione e le opzioni. Vediamo un esempio pratico utilizzando la stringa "Hello World" come input:

```Elixir
iex> String.replace("Hello World", "World", "Elixir")
"Hello Elixir"
```

Come puoi vedere, il pattern "World" viene sostituito con "Elixir" nella stringa di input. Inoltre, puoi utilizzare opzioni per personalizzare il comportamento della funzione. Ad esempio, puoi impostare l'opzione `global: true` per sostituire tutte le occorrenze del pattern invece di solo la prima.

Puoi anche utilizzare espressioni regolari come pattern per la ricerca e la sostituzione del testo. Ad esempio, se vuoi sostituire tutte le vocali in una stringa con degli asterischi, puoi farlo utilizzando una regex come pattern:

```Elixir
iex> String.replace("Hello World", ~r/[aeiou]/, "*")
"H*ll* W*rld"
```

Sperimenta con diverse opzioni e espressioni regolari per ottenere i risultati desiderati.

## Approfondimento

Oltre alla funzione `String.replace/4`, in Elixir ci sono altre funzioni utili per la ricerca e la sostituzione del testo, come `String.replace_leading/3` e `String.replace_trailing/3`. Queste funzioni consentono di specificare se si desidera sostituire solo il primo o l'ultimo match del pattern nella stringa. Inoltre, puoi anche utilizzare il modulo `Regex` per creare e utilizzare espressioni regolari più complesse.

Una cosa importante da tenere a mente è che le stringhe in Elixir sono immutabili, quindi ogni volta che utilizzi una funzione di ricerca e sostituzione, verrà creata e restituita una nuova stringa. Ciò significa che se hai bisogno di effettuare molte sostituzioni in una stringa, potresti voler considerare l'utilizzo di una libreria come `String.replace_stream/4`, che restituisce uno stream di stringhe senza creare copie multiple.

## Vedi anche

- [Documentazione di Elixir sulla funzione String.replace/4](https://hexdocs.pm/elixir/String.html#replace/4)
- [Regex in Elixir](https://hexdocs.pm/elixir/Regex.html)
- [Elixir String Cheat Sheet](https://devhints.io/elixir-string)