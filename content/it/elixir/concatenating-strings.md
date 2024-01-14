---
title:                "Elixir: Concatenazione di stringhe"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Con il linguaggio di programmazione Elixir, possiamo unire più stringhe di testo per creare una stringa più lunga. Questo può essere utile in molte situazioni come ad esempio la formattazione di output o la creazione di messaggi personalizzati.

## Come fare

Per concatenare stringhe in Elixir, possiamo utilizzare l'operatore `<>`. Possiamo anche utilizzare la funzione `String.concat/1` o il modulo `String` con il metodo `join/2`. Ecco alcuni esempi:

```
Elixir
# Utilizzando l'operatore <>
"Benvenuto " <> "in Elixir" 
# Output: "Benvenuto in Elixir"

# Utilizzando la funzione String.concat/1
String.concat(["Ciao", "Mondo"])
# Output: "CiaoMondo"

# Utilizzando il modulo String e il metodo join/2
String.join(["Questo", "è", "un", "esempio"])
# Output: "Questo è un esempio"
```

## Approfondimento

In Elixir, le stringhe sono immutabili, il che significa che non possiamo modificarle direttamente. Quando concateniamo le stringhe, in realtà stiamo creando una nuova stringa. Pertanto, è importante tenere in considerazione le prestazioni quando si uniscono più stringhe in un unico passaggio.

Inoltre, è importante notare che quando si utilizza l'operatore `<>`, Elixir effettua una conversione implicita delle stringhe in binario, mentre la funzione `String.concat/1` e il metodo `join/2` di `String` accettano una lista di elementi di qualsiasi tipo e li convertono in stringhe. Questo può essere utile quando si desidera concatenare diversi tipi di dati.

## Vedi anche

- [Documentazione ufficiale di Elixir sulle stringhe](https://hexdocs.pm/elixir/String.html)
- [Altri esempi di concatenazione di stringhe in Elixir](https://pragmaticstudio.com/tutorials/elixir-strings)
- [Una guida pratica all'uso delle stringhe in Elixir](https://medium.com/@tyronemichael/using-strings-in-elixir-4f8db2499445)