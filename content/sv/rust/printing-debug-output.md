---
title:                "Rust: Utskrift av debuggutdata"
programming_language: "Rust"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva kod är inte alltid en enkel uppgift och ibland behöver vi hjälp för att hitta buggar och fel i vår kod. Ett bra sätt att göra detta är genom att skriva ut felsökningsmeddelanden, eller "debug output", i vår kod. Det kan hjälpa oss att förstå hur vår kod fungerar och hitta eventuella problem som behöver lösas.

## Hur man gör det

För att skriva ut felsökningsmeddelanden behöver vi använda ett inbyggt makro i Rust som kallas "println!". Det fungerar på samma sätt som "print" eller "println" i andra programmeringsspråk, men har några extra funktioner som är specifika för Rust.

Enklast är att använda detta makro inuti en "main" funktion, men det kan också användas på andra platser i koden. Här är ett exempel på hur du kan skriva ut ett meddelande:

```
fn main() {
    let num = 5;
    println!("Värdet på num är {}", num);
}
```

Körexemplet skulle ge följande output:

```
Värdet på num är 5
```

"{}" fungerar som en platsmarkör där värdet av "num" kommer att ersätta platsmarkören när koden körs.

## Djupdykning

Det finns flera sätt att anpassa hur ett felsökningsmeddelande ser ut, till exempel genom att inkludera datatypen eller ändra formateringen av värdena. Detta kan vara användbart när vi behöver skriva ut mer komplex data.

En annan användbar funktion är "eprintln!" som kan användas för att skriva ut felmeddelanden. Detta gör det lättare att hitta och åtgärda eventuella fel i koden.

## Se även

- [Rust makroer](https://doc.rust-lang.org/book/ch19-06-macros.html)
- [Syntax Guide för println! Makrot](https://doc.rust-lang.org/std/macro.println.html)
- [Rust felsökning](https://rust-lang.github.io/rustup/concepts/debugging.html)