---
title:                "Utskrift av felsökningsutdata"
html_title:           "Rust: Utskrift av felsökningsutdata"
simple_title:         "Utskrift av felsökningsutdata"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva ut debug-utmatning är en viktig del av programmering eftersom det hjälper oss att förstå vad som händer i koden och felsöka eventuella problem. Det låter oss också se värdet på variabler och annan data för att kontrollera om vår kod fungerar som den ska.

## Så här gör du

För att skriva ut debug-utmatning i Rust använder vi makron `println!()` och `eprintln!()` för att skriva ut till standardutmatningen (stdout) och standardfelutmatningen (stderr) respektive. Här är ett enkelt exempel på hur du kan använda dessa makron:

```Rust
let num = 5;
let text = "Hello world!";
println!("Värdet på num är {}", num);
eprintln!("Texten är: {}", text);
```

Detta kommer att skriva ut "Värdet på num är 5" till vår stdout och "Texten är: Hello world!" till vår stderr. Du kan också använda formatering i `println!()` och `eprintln!()` på samma sätt som i `format!()` makron.

Du kan också använda `dbg!()` makron för att automatiskt skriva ut värdet på ett uttryck och dess typ till stdout. Här är ett exempel på hur man kan använda det:

```Rust
let num1 = 10;
let num2 = 5;
dbg!(num1 + num2);
```

Detta skulle skriva ut "num1 + num2: 15" till vår stdout. Detta kan vara mycket praktiskt när du behöver kontrollera värdet på variabler i ett större kodblock.

## Deep Dive

För att kontrollera när debug-utmatning ska skrivas ut kan vi använda `cfg!()` makron tillsammans med `println!()` och `eprintln!()`. `cfg!()` funktionen tar en konstant som argument och returnerar `true` om den konstanten är definierad och `false` annars. Här är ett exempel på hur vi kan använda dessa tillsammans:

```Rust
cfg!(debug_assertions) // Kontrollerar om debug-assertions är påslaget
```

Vilket betyder att vi bara skriver ut debug-utmatning om debug-assertions är påslaget, vilket är standarden för utvecklingsmiljön. Vi kan också använda `println!()` och `eprintln!()` för att skriva ut olika meddelanden beroende på vilken miljö vår kod körs i. Till exempel kan vi skriva ut felmeddelanden till vår stderr i produktionsmiljön istället för att skriva ut till stdout.

## Se också

Här är några ytterligare resurser du kan utforska för att lära dig mer om felsökning och användning av debug-utmatning i Rust:

- [Rust Language Reference – Macros](https://doc.rust-lang.org/reference/macros.html)
- [The Rust Programming Language – Debugging](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html)