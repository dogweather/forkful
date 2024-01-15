---
title:                "Unire stringhe"
html_title:           "Elixir: Unire stringhe"
simple_title:         "Unire stringhe"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molte situazioni in cui potresti aver bisogno di unire due stringhe in un'unica stringa più lunga. Potresti voler unire il nome e il cognome di una persona per creare un saluto personalizzato oppure potresti avere bisogno di unire diverse informazioni per creare una query per il database.

## Come Fare
Per unire due stringhe in Elixir, puoi utilizzare l'operatore `<>`, conosciuto come operatore di concatenazione. Inserisci semplicemente le due stringhe che vuoi unire all'interno delle parentesi quadre e separale dall'operatore di concatenazione. Vediamo un esempio:

```Elixir
nome = "Maria"
cognome = "Rossi"
saluto = nome <> " " <> cognome
```

In questo esempio, abbiamo creato due stringhe `nome` e `cognome` e le abbiamo unite insieme utilizzando l'operatore `<>`. La variabile `saluto` conterrà la stringa "Maria Rossi". Nota che l'operatore `<>` funziona solo con le stringhe, quindi se stai cercando di unire altri tipi di dati, come numeri o liste, dovrai utilizzare una diversa funzione.

## Approfondimento
Elixir è un linguaggio di programmazione funzionale, il che significa che le stringhe sono immutabili. Quindi, quando utilizzi l'operatore di concatenazione, in realtà stai creando una nuova stringa e non modificando quelle esistenti. Se hai bisogno di effettuare un grande numero di concatenazioni, potresti notare un degrado delle prestazioni a causa della creazione di molte nuove stringhe. In questi casi, una soluzione più efficiente potrebbe essere utilizzare il modulo `Enum` e la sua funzione `join` per unire una lista di stringhe.

Esistono anche altre funzioni in Elixir per manipolare e unire stringhe, come ad esempio `String.concat` o `String.replace`. Se vuoi approfondire ulteriormente sull'utilizzo delle stringhe in Elixir, puoi consultare la documentazione ufficiale del linguaggio.

## Vedi Anche
- Documentazione ufficiale di Elixir sulle stringhe: https://hexdocs.pm/elixir/String.html
- Blog post su come utilizzare le stringhe in Elixir: https://sentinelle.tech/blog/using-string-module-elixir/
- Video tutorial su Elixir e le stringhe: https://youtu.be/v5xFlkitJlA