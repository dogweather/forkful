---
title:    "Elixir: Il concatenamento di stringhe"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Perché
Concatenare le stringhe è una tecnica molto utile quando si lavora con dati di testo in Elixir. Può essere utilizzato per combinare più stringhe in una sola o per formattare una stringa con altre variabili o valori.

## Come Fare
Per concatenare le stringhe in Elixir, è possibile utilizzare l'operatore `<>` o la funzione `String.concat/1`. Vediamo un esempio di entrambi i modi:

```
# Utilizzando l'operatore <>
"Il mio numero preferito è il " <> '42'
# Output: Il mio numero preferito è il 42

# Utilizzando la funzione String.concat/1
String.concat(["Ciao", " ", "amici"])
# Output: Ciao amici
```

Inoltre, è possibile concatenare qualsiasi tipo di dato, non solo stringhe. Quando viene utilizzato l'operatore `<>`, verrà convertito in una stringa prima della concatenazione.

## Approfondimento
Concatenare le stringhe può sembrare un'operazione semplice, ma ci sono alcuni aspetti da considerare quando si lavora con dati di testo in Elixir. Ad esempio, quando si concatenano più stringhe usando l'operatore `<>`, verranno create più istanze della stessa stringa in memoria. Questo può essere inefficiente in termini di utilizzo della memoria. D'altra parte, utilizzando la funzione `String.concat/1`, verrà creata una sola stringa in memoria.

Inoltre, Elixir dispone di una libreria di formattazione delle stringhe chiamata [String](https://hexdocs.pm/elixir/String.html) che offre diverse funzioni utili per manipolare e formattare le stringhe. Consiglio di dargli un'occhiata per approfondire ulteriormente l'utilizzo delle stringhe in Elixir.

## Vedi Anche
- [Elixir string concatenation vs interpolation](https://cultofthepartyparrot.com/elilixir-string-concatenation-vs-interpolation/)
- [Elixir String library](https://hexdocs.pm/elixir/String.html)