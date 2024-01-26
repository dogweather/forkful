---
title:                "Lettura degli argomenti della riga di comando"
date:                  2024-01-20T17:56:09.251711-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Leggere gli argomenti della linea di comando consente ai programmi di accettare input direttamente al lancio, fornendo flessibilità e controllo all'utente. I programmatori lo utilizzano per personalizzare l'esecuzione o gestire configurazioni senza hard-coded rigidity.

## How to:
In Gleam, puoi leggere gli argomenti dalla linea di comando usando `os.args()`:

```gleam
import gleam/io
import gleam/os

pub fn main() {
  let args = os.args()
  case args {
    [] -> io.println("Mi dispiace, non hai fornito argomenti.")
    [a] -> io.println("Hai fornito un argomento: " ++ a)
    _ -> io.println("Wow, tanti argomenti! Eccoli: " ++ list.join(args, ", "))
  }
}
```

Running `gleam run` potrebbe darti:
```
Hai fornito un argomento: foo
```
O:
```
Wow, tanti argomenti! Eccoli: foo, bar, baz
```

## Deep Dive
Prima di Gleam, la lettura degli argomenti della linea di comando era una pratica comune in altri linguaggi come C e Python. In Erlang, si utilizza `init:get_args()` che è più verboso. Gleam prende spunto ma semplifica la sintassi, rendendola più accessibile.

Alternativamente, potresti usare librerie esterne per funzionalità avanzate, come il parsing di flag o la validazione degli argomenti. La libreria standard, tuttavia, è sufficiente per molti casi d'uso.

Dal punto di vista implementativo, `os.args()` in Gleam passa attraverso la virtual machine BEAM, sfruttando le ottimizzazioni di Erlang/OTP per gestire gli input esterni in modo robusto ed efficiente.

## See Also
- Gleam documentation: [https://gleam.run/](https://gleam.run/)
- Erlang's `init` module: [https://erlang.org/doc/man/init.html](https://erlang.org/doc/man/init.html)
