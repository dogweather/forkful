---
title:                "Elixir: Incorporare stringhe"
simple_title:         "Incorporare stringhe"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché
La concatenazione di stringhe è una pratica comune nella programmazione Elixir. Essa permette di unire due o più stringhe in una sola, il che può essere molto utile per creare output dinamici o manipolare dati. 

## Come Fare
Per concatenare stringhe in Elixir, è possibile utilizzare l'operatore "+" o la funzione `String.concat/2`. Di seguito un esempio di entrambi i metodi:

```
Elixir iex> "Ciao" + "Mondo"
"CiaoMondo"
Elixir iex> String.concat("Buongiorno", "a tutti!")
"Buongiorno a tutti!"
```

In questo esempio vediamo come sia possibile concatenare qualsiasi tipo di dato in Elixir, non solo le stringhe. Ad esempio:

```
Elixir iex> "Edward " + 007
"Edward 007"
Elixir iex> String.concat([1, 2, 3], " numeri")
"123 numeri"
```

## Deep Dive
Nel linguaggio di programmazione Elixir, le stringhe sono immutabili, il che significa che non possono essere modificate direttamente. Questo significa che ogni volta che viene fatta una concatenazione di stringhe, ne viene creata una nuova. Ciò può diventare un problema di prestazioni quando si manipolano una grande quantità di dati. 

Per risolvere questo problema, esiste la funzione `Enum.reduce/2` che può essere utilizzata per concatenare grandi quantità di stringhe in modo più efficiente, evitando la creazione di nuove stringhe ad ogni iterazione.

```
Elixir iex> lista = ["Ciao", " ", "Mondo", "!"]
["Ciao", " ", "Mondo", "!"]
Elixir iex> Enum.reduce(lista, "", fn x, acc -> acc <> x end)
"Ciao Mondo!"
```

## See Also
- [Documentazione ufficiale di Elixir su stringhe](https://hexdocs.pm/elixir/String.html)
- [Articolo sull'utilizzo di `Enum.reduce/2` per la concatenazione di stringhe](https://elixirschool.com/it/lessons/advanced/enumerables/#Enum.reduce) 
- [Tutorial interattivo su Elixir](https://elixir-lang.org/learning.html)