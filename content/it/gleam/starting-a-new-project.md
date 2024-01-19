---
title:                "Iniziare un nuovo progetto"
html_title:           "Arduino: Iniziare un nuovo progetto"
simple_title:         "Iniziare un nuovo progetto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Avviare un nuovo progetto di programmazione significa creare da zero un nuovo software. I programmatori lo fanno per risolvere problemi specifici, innovare o semplicemente per apprendere nuove tecniche.

## Come fare:

Creare un nuovo progetto in Gleam è semplice. Ecco un esempio di come fare:

```Gleam
$ gleam new ciao_mondo
$ cd ciao_mondo
```

Dopo aver eseguito questi comandi, avrete un nuovo progetto Gleam chiamato "ciao_mondo". Per eseguire il progetto, utilizzate il seguente comando:

```Gleam
$ rebar3 shell
1> ciao_mondo:inizia().
Ciao, Mondo!
```

## Approfondimento

Quando avviate il nuovo progetto, Gleam crea una serie di file e cartelle per voi. Questo design di base del progetto è stato sviluppato nel corso degli anni per seguire le migliori pratiche. Ci sono alternative come `Mix` per Elixir e `rebar3` per Erlang, ma `gleam new` è un ottimo inizio per un nuovo progetto Gleam.

Gleam è un linguaggio di programmazione staticamente tipizzato progettato per il BEAM, la macchina virtuale Erlang. Nonostante sia relativamente giovane, sta guadagnando popolarità grazie alla sua sintassi chiara e al forte sistema di tipi.

## Vedi Anche:

Per ulteriori informazioni su Gleam, potete consultare le seguenti risorse:

1. [La documentazione ufficiale di Gleam](https://gleam.run/docs/)
2. [Una guida introduttiva a Gleam](https://gleam.run/getting-started/)
3. [Il repository GitHub di Gleam](https://github.com/gleam-lang/gleam)

Ricorda, la miglior maniera per imparare è fare pratica, quindi iniziate il vostro nuovo progetto Gleam oggi stesso!