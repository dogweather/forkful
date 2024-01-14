---
title:    "Gleam: Il leggere gli argomenti della riga di comando"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Perché

Molti programmatori sono abituati a interagire con i propri programmi attraverso un'interfaccia grafica, ma ci sono situazioni in cui è necessario utilizzare la riga di comando per passare informazioni al programma. In questo articolo, esploreremo come leggere gli argomenti della riga di comando utilizzando il linguaggio di programmazione Gleam.

## Come fare

Per iniziare, è necessario creare un file Gleam con una funzione che accetta gli argomenti della riga di comando come parametro. Ad esempio:

```Gleam
import gleam/io

pub fn main(command_line_args: List(String)) {
  // codice per gestire gli argomenti della riga di comando
}
```

All'interno di questa funzione, è possibile utilizzare la funzione `gleam/io.from_command_line_args/0` per ottenere gli argomenti passati al programma come una lista di stringhe. Ad esempio:

```Gleam
fn main(command_line_args) {
  let argumenti = gleam/io.from_command_line_args()
  gleam/io.print(argumenti)
}
```

Se eseguiamo questo programma passando "arg1" e "arg2" come argomenti della riga di comando, vedremo l'output "['arg1', 'arg2']".

## Approfondimento

Ora che sappiamo come ottenere gli argomenti della riga di comando, possiamo approfondire con alcune funzioni utili per gestirli. Ad esempio, la funzione `gleam/list.head/1` restituisce il primo elemento di una lista, mentre `gleam/list.tail/1` restituisce tutti gli elementi tranne il primo. In combinazione, queste funzioni possono essere utili per estrarre argomenti specifici dalla lista degli argomenti della riga di comando.

Inoltre, Gleam fornisce diverse funzioni per convertire le stringhe in altri tipi di dati, come `gleam/string.to_uint/1` per convertire una stringa in un intero non segnato. Queste funzioni possono essere utili quando gli argomenti della riga di comando devono essere utilizzati in operazioni matematiche o confronti.

##Vedi anche

Per ulteriori informazioni su come gestire gli argomenti della riga di comando in Gleam, puoi consultare la documentazione ufficiale sulle funzioni `gleam/io` e `gleam/list`.

- [Documentazione Gleam: gleam/io](https://gleam.run/documentation/stdlib/io.html)
- [Documentazione Gleam: gleam/list](https://gleam.run/documentation/stdlib/list.html)