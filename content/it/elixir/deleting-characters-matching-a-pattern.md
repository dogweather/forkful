---
title:    "Elixir: Cancellando caratteri che corrispondono a un modello."
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Quando si lavora con il linguaggio di programmazione Elixir, potresti trovarti nella situazione in cui devi eliminare dei caratteri che corrispondono ad un certo pattern all'interno di una stringa. Ci possono essere diversi motivi per farlo, come ad esempio la pulizia dei dati prima di elaborarli o la rimozione di caratteri speciali. In questo articolo, spiegheremo come eseguire questa operazione utilizzando Elixir.

## Come fare

Per eliminare i caratteri corrispondenti ad un pattern in Elixir, possiamo utilizzare il metodo `String.replace/3` che prende in input la stringa da modificare, il pattern da cercare e la stringa di sostituzione. Vediamo un esempio pratico:

```Elixir
stringa = "Questa è una frase con caratteri speciali!!!"
String.replace(stringa, ~r/[^a-zA-Z\s]/, "")
```

In questo esempio stiamo eliminando tutti i caratteri che non sono lettere o spazi all'interno della stringa, ottenendo come risultato "Questa una frase con caratteri speciali". Possiamo anche utilizzare espressioni regolari più complesse per cercare caratteri specifici e sostituirli con un altro carattere.

## Approfondimento

Oltre al metodo `String.replace/3`, esistono altre funzioni utili per la manipolazione delle stringhe in Elixir. Ad esempio, la funzione `String.trim/2` rimuove gli spazi all'inizio e alla fine della stringa, mentre `String.downcase/1` converte tutti i caratteri della stringa in minuscolo. È importante ricordare che, a differenza di altri linguaggi di programmazione, le stringhe in Elixir sono immutabili, quindi tutte queste funzioni restituiscono una nuova stringa senza modificare quella originale.

Se vuoi saperne di più sulla gestione delle stringhe in Elixir, ti consigliamo di consultare la documentazione ufficiale e di esplorare le diverse funzioni messe a disposizione dal linguaggio.

## Vedi anche

- Documentazione ufficiale di Elixir sui tipi di dati delle stringhe: https://hexdocs.pm/elixir/String.html
- Tutorial su come utilizzare le espressioni regolari in Elixir: https://weblog.jamisbuck.org/2015/9/12/elixir-regexes-in-depth.html
- Video tutorial su come manipolare le stringhe in Elixir: https://www.youtube.com/watch?v=lW9VBp4uESo