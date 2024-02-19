---
aliases:
- /it/rust/using-a-debugger/
date: 2024-01-26 04:10:00.652408-07:00
description: "Utilizzare un debugger \xE8 come dotarsi di una visione a raggi X per\
  \ sbirciare nell'esecuzione del proprio codice. I programmatori lo fanno per individuare\u2026"
lastmod: 2024-02-18 23:08:55.684584
model: gpt-4-0125-preview
summary: "Utilizzare un debugger \xE8 come dotarsi di una visione a raggi X per sbirciare\
  \ nell'esecuzione del proprio codice. I programmatori lo fanno per individuare\u2026"
title: Utilizzo di un debugger
---

{{< edit_this_page >}}

## Cos'è & Perché?

Utilizzare un debugger è come dotarsi di una visione a raggi X per sbirciare nell'esecuzione del proprio codice. I programmatori lo fanno per individuare bug, comprendere il flusso del programma e assicurarsi che il loro codice sia pulito come un fischietto. È come avere un amico che indica esattamente dove si è inciampato.

## Come fare:

Rust supporta vari debugger, ma quelli comuni sono `gdb` per GNU/Linux o `lldb` per macOS. Potresti anche usare `rust-gdb` o `rust-lldb`, che sono wrapper che visualizzano in modo più chiaro i valori Rust. Ecco un'anteprima:

```Rust
fn main() {
    let mut counter = 0;
    for _ in 0..5 {
        counter += 1;
        println!("Il contatore è a: {}", counter);
    }
}
```

Per eseguire il debug, compila con le informazioni di debug:

```shell
$ rustc -g counter.rs
```

Poi eseguilo in `rust-gdb`:

```shell
$ rust-gdb counter
(gdb) break main
(gdb) run
(gdb) print counter
$1 = 0
(gdb) continue
Il contatore è a: 1
(gdb) print counter
$2 = 1
```

## Approfondimento

Il debug esiste fin dai *tempi antichi* delle schede perforate, e la sua evoluzione è stata una manna dal cielo. Rust fornisce il proprio strumentario con integrazioni per GDB e LLDB a causa della natura a livello di sistema del linguaggio.

Le alternative per il debug del codice Rust includono l'uso degli ambienti di sviluppo integrati (IDE) con i loro debugger incorporati, che alcuni trovano più intuitivi. Tra i più popolari ci sono CLion con il plugin Rust o Visual Studio Code con l'estensione Rust.

Per quanto riguarda l'implementazione, Rust genera simboli di debug che questi debugger comprendono, il che è fondamentale per passare attraverso il codice, impostare breakpoint e ispezionare le variabili senza perdere la bussola.

## Vedi anche

- Il libro di Rust sul Debugging: https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html#guidelines-for-error-handling
- Rust By Example sulla Gestione degli Errori e il Debugging: https://doc.rust-lang.org/rust-by-example/error.html
- Il Rust Language Server (RLS) che alimenta l'estensione Rust di VS Code: https://github.com/rust-lang/rls
- Debugging Rust con Visual Studio Code: https://marketplace.visualstudio.com/items?itemName=rust-lang.rust
