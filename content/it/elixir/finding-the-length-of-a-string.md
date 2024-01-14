---
title:    "Elixir: Trova la lunghezza di una stringa"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Perché

Trovare la lunghezza di una stringa è un'operazione comune nella programmazione. Sapere come farlo può semplificare il processo di lavoro e aiutare a creare codice più efficiente.

## Come fare

Per iniziare, l'utilizzo della funzione `String.length()` è il modo più semplice per ottenere la lunghezza di una stringa in Elixir. Questa funzione accetta una stringa come argomento e restituisce la sua lunghezza.

```Elixir
stringa = "Ciao mondo"
String.length(stringa)
```

L'output sarà `11`, poiché ci sono 11 caratteri nella stringa, inclusi lo spazio e la lettera "ò".

Per rendere il processo più dinamico, è possibile creare una funzione personalizzata che accetta una stringa e restituisce la sua lunghezza.

```Elixir
def lunghezza_stringa(stringa) do
  String.length(stringa)
end

lunghezza_stringa("Questo è un esempio di stringa") # 28
```

È importante notare che, poiché in Elixir le stringhe sono immutabili, la funzione `String.length()` non modificare la stringa originale.

## Approfondimento

In Elixir, le stringhe sono codificate in UTF-8, che utilizza più byte per rappresentare caratteri non ASCII rispetto all'encoding ASCII. Ciò significa che la funzione `String.length()` restituirà la lunghezza in byte della stringa e non il numero di caratteri visibili.

Per ottenere il numero di caratteri visibili in una stringa, è possibile utilizzare la funzione `String.graphemes()` che restituisce una lista di caratteri.

```Elixir
stringa = "Olà"
String.length(stringa) # 5
String.graphemes(stringa) # ["O", "l", "à"]
```

Se si vuole ottenere il numero di byte effettivo della stringa, è possibile utilizzare la funzione `byte_size()`.

```Elixir
stringa = "Ciao 👋"
byte_size(stringa) # 7
```

## Vedi anche

- [Elixir String Module]("https://hexdocs.pm/elixir/String.html")
- [UTF-8 Encoding]("https://en.wikipedia.org/wiki/UTF-8")