---
date: 2024-01-26 04:10:21.316599-07:00
description: "U\u017Cycie debugera to jak przyznanie sobie rentgenowskiego wzroku,\
  \ aby zajrze\u0107 do wykonania twojego kodu. Programi\u015Bci robi\u0105 to, aby\
  \ wykry\u0107 b\u0142\u0119dy, zrozumie\u0107\u2026"
lastmod: '2024-02-25T18:49:33.556574-07:00'
model: gpt-4-0125-preview
summary: "U\u017Cycie debugera to jak przyznanie sobie rentgenowskiego wzroku, aby\
  \ zajrze\u0107 do wykonania twojego kodu. Programi\u015Bci robi\u0105 to, aby wykry\u0107\
  \ b\u0142\u0119dy, zrozumie\u0107\u2026"
title: Korzystanie z debugera
---

{{< edit_this_page >}}

## Co i dlaczego?

Użycie debugera to jak przyznanie sobie rentgenowskiego wzroku, aby zajrzeć do wykonania twojego kodu. Programiści robią to, aby wykryć błędy, zrozumieć przepływ programu i upewnić się, że ich kod jest czysty jak gwizdek. To jak posiadanie kumpla, który wskazuje dokładnie, gdzie się potknąłeś.

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
