---
date: 2024-01-26 04:10:31.875153-07:00
description: "Rust st\xF6djer olika fels\xF6kare, men en vanlig \xE4r `gdb` f\xF6\
  r GNU/Linux eller `lldb` f\xF6r macOS. Du kan ocks\xE5 anv\xE4nda `rust-gdb` eller\
  \ `rust-lldb` som \xE4r\u2026"
lastmod: '2024-03-13T22:44:37.702970-06:00'
model: gpt-4-0125-preview
summary: "Rust st\xF6djer olika fels\xF6kare, men en vanlig \xE4r `gdb` f\xF6r GNU/Linux\
  \ eller `lldb` f\xF6r macOS. Du kan ocks\xE5 anv\xE4nda `rust-gdb` eller `rust-lldb`\
  \ som \xE4r\u2026"
title: "Att anv\xE4nda en debugger"
weight: 35
---

## Hur man gör:
Rust stödjer olika felsökare, men en vanlig är `gdb` för GNU/Linux eller `lldb` för macOS. Du kan också använda `rust-gdb` eller `rust-lldb` som är inpackningar som gör att Rust-värden skrivs ut på ett lättläst sätt. Här är en inblick:

```Rust
fn main() {
    let mut counter = 0;
    for _ in 0..5 {
        counter += 1;
        println!("Counter is at: {}", counter);
    }
}
```

För att felsöka detta, kompilera med debuginformation:

```shell
$ rustc -g counter.rs
```

Kör sedan det i `rust-gdb`:

```shell
$ rust-gdb counter
(gdb) break main
(gdb) run
(gdb) print counter
$1 = 0
(gdb) continue
Counter is at: 1
(gdb) print counter
$2 = 1
```

## Djupdykning
Felsökning har funnits sedan *de gamla goda tiderna* med hålkort, och dess utveckling har varit en gudagåva. Rust erbjuder sin egen verktygslåda med integreringar för GDB och LLDB på grund av språkets systemnivåkaraktär.

Alternativ för att felsöka Rust-kod inkluderar användning av integrerade utvecklingsmiljöer (IDE) med deras inbyggda felsökare, vilket vissa finner mer intuitivt. Populära val inkluderar CLion med Rust-tillägget eller Visual Studio Code med Rust-förlängningen.

När det gäller implementering genererar Rust felsökningssymboler som dessa felsökare förstår, vilket är avgörande för att stega genom koden, sätta brytpunkter och inspektera variabler utan att förlora förståndet.

## Se även
- Rust-boken om felsökning: https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html#guidelines-for-error-handling
- Rust By Example’s syn på fel och felsökning: https://doc.rust-lang.org/rust-by-example/error.html
- Rust Language Server (RLS) som driver VS Code's Rust-tillägg: https://github.com/rust-lang/rls
- Felsökning av Rust med Visual Studio Code: https://marketplace.visualstudio.com/items?itemName=rust-lang.rust
