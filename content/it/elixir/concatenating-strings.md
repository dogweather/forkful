---
title:    "Elixir: Incatenando stringhe"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Concatenare stringhe è una tecnica fondamentale nella programmazione Elixir. È utile per combinare più stringhe in una sola, che può poi essere utilizzata per scopi come la stampa di un output o la creazione di URL. Ma perché esattamente dovresti imparare a concatenare stringhe? Semplicemente perché è uno strumento essenziale per gestire e manipolare i dati all'interno dei tuoi programmi.

## Come fare

Per concatenare stringhe in Elixir, utilizziamo l'operatore `<>` che unisce due stringhe in una sola. Vediamo un esempio di codice:

```Elixir
nome = "Marco"
cognome = "Rossi"
nome_completo = nome <> " " <> cognome
IO.puts nome_completo
```

Nell'esempio precedente, abbiamo creato due stringhe con le variabili `nome` e `cognome`. Poi, abbiamo concatenato le due stringhe utilizzando l'operatore `<>` e salvato il risultato nella variabile `nome_completo`. Infine, abbiamo utilizzato la funzione `IO.puts` per stampare il nome completo sulla console. L'output di questo codice sarebbe "Marco Rossi".

## Approfondimento

È importante notare che l'operatore `<>` non solo concatena due stringhe, ma converte anche tutti i valori in stringhe. Ad esempio, se provassimo ad utilizzare l'operatore con un numero e una stringa, il numero sarebbe convertito in una stringa prima della concatenazione. Inoltre, è possibile concatenare più di due stringhe in una sola volta, basta inserire l'operatore tra ciascuna di esse.

## Vedi anche

- [Documentazione ufficiale di Elixir](https://elixir-lang.org/docs.html)
- [Tutorial di Elixir per principianti](https://www.tutorialspoint.com/elixir/index.htm)
- [Articolo su come gestire le stringhe in Elixir](https://medium.com/@poeticoding/string-manipulation-in-elixir-ba03cb3c4cf3)