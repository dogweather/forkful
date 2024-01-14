---
title:    "Elixir: Capitalizzazione di una stringa"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Perché

Capitalize una stringa è un'operazione comune in molte applicazioni di programmazione. È utile per modificare il modo in cui una stringa viene visualizzata all'utente o per elaborare dati in un determinato formato.

## Come fare

Per capitalizzare una stringa in Elixir, è possibile utilizzare la funzione built-in `String.capitalize/1`. Ad esempio:

```Elixir
String.capitalize("ciao") #=> "Ciao"
```

Se si desidera capitalizzare ogni parola in una stringa, è possibile utilizzare la funzione `String.capitalize_words/1`:

```Elixir
String.capitalize_words("ciao mondo") #=> "Ciao Mondo"
```

In entrambi i casi, la funzione restituirà una nuova stringa con la prima lettera di ogni parola in maiuscolo.

## Approfondimento

La funzione `String.capitalize/1` utilizza il concetto di funzionalità di Elixir chiamato "protocolli". Questi consentono di fornire diverse implementazioni della stessa funzione per tipi di dati diversi. Nel caso di `String.capitalize/1`, esiste un protocollo chiamato `String.Chars` che definisce come le stringhe dovrebbero essere convertite in formato "ciao" (in minuscolo) e viceversa.

Se si desidera una maggiore flessibilità nella capitalizzazione di stringhe, è possibile implementare il protocollo `String.Chars` per creare una logica personalizzata per il proprio tipo di dato.

## Vedi anche

- [Documentazione di Elixir sulla funzione `String.capitalize/1`](https://hexdocs.pm/elixir/String.html#capitalize/1)
- [Documentazione di Elixir sul protocollo `String.Chars`](https://hexdocs.pm/elixir/String.Chars.html)