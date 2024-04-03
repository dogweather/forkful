---
date: 2024-01-26 04:10:21.316599-07:00
description: "Jak to zrobi\u0107: Rust wspiera r\xF3\u017Cne debugery, ale popularnym\
  \ jest `gdb` dla GNU/Linuxa lub `lldb` dla macOS. Mo\u017Cna tak\u017Ce u\u017C\
  y\u0107 `rust-gdb` lub `rust-lldb`,\u2026"
lastmod: '2024-03-13T22:44:35.189496-06:00'
model: gpt-4-0125-preview
summary: "Rust wspiera r\xF3\u017Cne debugery, ale popularnym jest `gdb` dla GNU/Linuxa\
  \ lub `lldb` dla macOS."
title: Korzystanie z debugera
weight: 35
---

## Jak to zrobić:
Rust wspiera różne debugery, ale popularnym jest `gdb` dla GNU/Linuxa lub `lldb` dla macOS. Można także użyć `rust-gdb` lub `rust-lldb`, które są wrapperami, formatującymi wartości Rusta w przyjazny sposób. Oto przykład:

```Rust
fn main() {
    let mut counter = 0;
    for _ in 0..5 {
        counter += 1;
        println!("Licznik jest na: {}", counter);
    }
}
```

Aby to zdebugować, skompiluj z informacjami debugowania:

```shell
$ rustc -g counter.rs
```

Następnie uruchom to w `rust-gdb`:

```shell
$ rust-gdb counter
(gdb) break main
(gdb) run
(gdb) print counter
$1 = 0
(gdb) continue
Licznik jest na: 1
(gdb) print counter
$2 = 1
```

## Pogłębiona analiza
Debugowanie istnieje od czasów starych jak świat kart perforowanych, a jego ewolucja okazała się być darem niebios. Rust dostarcza własne narzędzia z integracjami dla GDB i LLDB ze względu na systemowy charakter języka.

Alternatywy dla debugowania kodu Rusta obejmują używanie zintegrowanych środowisk deweloperskich (IDE) ze wbudowanymi debuggerami, które niektórzy uważają za bardziej intuicyjne. Popularne to CLion z pluginem Rusta lub Visual Studio Code z rozszerzeniem Rusta.

Jeśli chodzi o implementację, Rust generuje symbole debugowania, które te debugery rozumieją, co jest kluczowe do przestępowania przez kod, ustawiania punktów przerwania i inspekcji zmiennych bez tracenia zmysłów.

## Zobacz także
- Rust Book o debugowaniu: https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html#guidelines-for-error-handling
- Podejście Rust By Example do błędów i debugowania: https://doc.rust-lang.org/rust-by-example/error.html
- Rust Language Server (RLS), który napędza rozszerzenie Rusta w VS Code: https://github.com/rust-lang/rls
- Debugowanie Rusta z Visual Studio Code: https://marketplace.visualstudio.com/items?itemName=rust-lang.rust
