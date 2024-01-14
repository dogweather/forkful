---
title:    "Elixir: Estrazione di sottostringhe"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte situazioni in cui è necessario e utile estrarre sottostringhe da una stringa più grande. Ad esempio, potresti voler manipolare una stringa controllando solo una parte di essa, o potresti dover trovare un determinato pattern all'interno di una stringa. In Elixir, esistono diversi metodi per estrarre sottostringhe e in questo articolo impareremo come farlo in modo semplice ed efficace.

## Come fare

Per estrarre una sottostringa in Elixir puoi utilizzare la funzione `String.slice/3`, passando tre argomenti: la stringa originale, l'indice di inizio e l'indice di fine della sottostringa. Di seguito un esempio pratico:

```Elixir
stringa_originale = "Ciao amici!"
sottostringa = String.slice(stringa_originale, 0, 4)
# output: "Ciao"
```

In questo esempio, abbiamo estratto i primi quattro caratteri dalla stringa originale. È importante notare che gli indici vengono conteggiati a partire da zero, quindi il carattere iniziale corrisponde all'indice 0. Inoltre, l'indice di fine è esclusivo, quindi nel nostro esempio l'ultimo carattere incluso nella sottostringa sarà il terzo (indice 3).

Puoi anche utilizzare gli operatori `..` e `..=` per specificare l'intervallo di caratteri da estrarre. Ad esempio:

```Elixir
sottostringa = String.slice(stringa_originale, 5..9)
# output: "amici"
sottostringa = String.slice(stringa_originale, 5..=9)
# output: "amic!"
```

In questo caso, con l'operatore `..` l'indice di fine è esclusivo, mentre con l'operatore `..=` è inclusivo.

Puoi anche utilizzare il terzo argomento della funzione `String.slice/3` per specificare un passo di incremento. Ad esempio:

```Elixir
sottostringa = String.slice(stringa_originale, 0, 10, 2)
# output: "Ca  i"
```

In questo caso, la sottostringa conterrà solo i caratteri a indici pari (0, 2, 4, ecc.).

## Approfondimento

Oltre alla funzione `String.slice/3`, esistono altri metodi per estrarre sottostringhe in Elixir. Ad esempio, puoi utilizzare le funzioni `String.split/3` e `String.replace/4` per dividere una stringa in base a un determinato delimitatore e per sostituire una sottostringa con un'altra.

Inoltre, nel linguaggio Elixir è presente la libreria `Regex` che permette di utilizzare le espressioni regolari per manipolare stringhe in modo ancora più potente ed efficace. Ad esempio, utilizzando la funzione `Regex.scan/3` puoi estrarre tutte le occorrenze di una determinata espressione regolare all'interno di una stringa e utilizzando la funzione `Regex.replace/3` puoi sostituire tali occorrenze con un'altra stringa o sottostringa.

In generale, si consiglia di familiarizzare con la libreria `Regex` per sfruttare al meglio la potenza delle espressioni regolari per la manipolazione delle stringhe in Elixir.

## Guarda anche

- Documentazione ufficiale su `String.slice/3`: https://hexdocs.pm/elixir/String.html#slice/3
- Documentazione ufficiale su `Regex`: https://hexdocs.pm/elixir/Regex.html
- Tutorial introduttivo sulle espressioni regolari in Elixir: https://elixir-lang.org/getting-started/regex.html