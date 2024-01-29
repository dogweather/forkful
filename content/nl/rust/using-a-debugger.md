---
title:                "Een debugger gebruiken"
date:                  2024-01-28T22:09:04.565659-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een debugger gebruiken"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/rust/using-a-debugger.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een debugger gebruiken is alsof je jezelf röntgenvisie geeft om in de uitvoering van je code te gluren. Programmeurs doen dit om bugs op te sporen, de programmastroom te begrijpen en ervoor te zorgen dat hun code zo schoon is als een fluitje. Het is alsof je een maatje hebt dat precies aanwijst waar je gestruikeld bent.

## Hoe te:

Rust ondersteunt verschillende debuggers, maar een veelvoorkomende is `gdb` voor GNU/Linux of `lldb` voor macOS. Je kunt ook `rust-gdb` of `rust-lldb` gebruiken, dit zijn wrappers die Rust-waarden mooi weergeven. Hier is een voorbeeld:

```Rust
fn main() {
    let mut counter = 0;
    voor _ in 0..5 {
        counter += 1;
        println!("Teller staat op: {}", counter);
    }
}
```

Om dit te debuggen, compileer met debuginformatie:

```shell
$ rustc -g counter.rs
```

Voer het vervolgens uit in `rust-gdb`:

```shell
$ rust-gdb counter
(gdb) break main
(gdb) run
(gdb) print counter
$1 = 0
(gdb) continue
Teller staat op: 1
(gdb) print counter
$2 = 1
```

## Diepgaand

Debuggen bestaat al sinds de *oude tijden* van ponskaarten, en de evolutie ervan is een godsgeschenk geweest. Rust biedt zijn eigen gereedschap met integraties voor GDB en LLDB vanwege de systeemniveau-aard van de taal.

Alternatieven voor het debuggen van Rust-code omvatten het gebruik van geïntegreerde ontwikkelomgevingen (IDE's) met hun ingebouwde debuggers, wat sommigen intuïtiever vinden. Populaire zijn onder andere CLion met de Rust-plug-in of Visual Studio Code met de Rust-uitbreiding.

Wat implementatie betreft, genereert Rust debugsymbolen die deze debuggers begrijpen, wat essentieel is voor het stapsgewijs doorlopen van de code, het instellen van breakpoints en het inspecteren van variabelen zonder je verstand te verliezen.

## Zie Ook

- Het Rust Boek over Debuggen: https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html#guidelines-for-error-handling 
- Rust By Example's kijk op Fouten en Debuggen: https://doc.rust-lang.org/rust-by-example/error.html
- De Rust Language Server (RLS) die de Rust-uitbreiding van VS Code aandrijft: https://github.com/rust-lang/rls
- Rust Debuggen met Visual Studio Code: https://marketplace.visualstudio.com/items?itemName=rust-lang.rust
