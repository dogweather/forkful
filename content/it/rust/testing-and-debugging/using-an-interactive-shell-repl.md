---
date: 2024-01-26 04:17:57.798919-07:00
description: 'Come fare: Al momento, Rust non ha un REPL ufficiale incluso. Puoi usare
  strumenti di terze parti come `evcxr_repl`. Installalo con Cargo.'
lastmod: '2024-03-13T22:44:43.219649-06:00'
model: gpt-4-0125-preview
summary: Al momento, Rust non ha un REPL ufficiale incluso.
title: Utilizzo di un interprete interattivo (REPL)
weight: 34
---

## Come fare:
Al momento, Rust non ha un REPL ufficiale incluso. Puoi usare strumenti di terze parti come `evcxr_repl`. Installalo con Cargo:

```sh
cargo install evcxr_repl
```

Poi, esegui il REPL:

```sh
evcxr
```

All'interno, testa del codice Rust:

```rust
let x = 5;
let y = 3;
println!("{} + {} = {}", x, y, x + y);
```

L'output dovrebbe essere:

```
5 + 3 = 8
```

## Approfondimento
L'etica di Rust si concentra su sicurezza e prestazioni, che sono solitamente associate a linguaggi compilati a priori, e meno a quelli interpretati, amichevoli al REPL. Storicamente, linguaggi come Python o Ruby hanno dato priorità all'avere un REPL per un feedback immediato, ma non sono stati progettati con in mente compiti a livello di sistema.

Nonostante l'assenza di un REPL ufficiale in Rust, sono emerse un paio di alternative come `evcxr_repl`. Questi progetti non stanno semplicemente adattando Rust a un REPL; stanno abilmente intrecciando insieme il ciclo di compilazione-ed-esecuzione del linguaggio in una sessione interattiva. Il REPL compila il codice dietro le quinte ed esegue il binario, catturando l'output. In questo modo, preserva i vantaggi delle prestazioni di Rust pur offrendo quell'esperienza interattiva.

C'è una discussione in corso nella comunità Rust riguardo al supporto ufficiale del REPL, e con ogni iterazione del linguaggio, vediamo una maggiore sofisticazione degli strumenti che potrebbe alla fine portare a una soluzione nativa.

## Vedi Anche
Per maggiori informazioni e altri strumenti:
- Repository GitHub di Evcxr REPL: [https://github.com/google/evcxr](https://github.com/google/evcxr)
- Rust Playground, un modo online per testare codice Rust: [https://play.rust-lang.org/](https://play.rust-lang.org/)
- Discussione sulla funzionalità REPL nel linguaggio Rust: [https://internals.rust-lang.org/](https://internals.rust-lang.org/)
