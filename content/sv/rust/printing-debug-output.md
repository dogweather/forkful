---
title:                "Rust: Utskrift av felsökningsutdata"
simple_title:         "Utskrift av felsökningsutdata"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

I många programmeringsspråk är det vanligt att använda utskrift och felmeddelanden för att felsöka kod. Men i Rust, är det inte lika vanligt. Så varför skulle man vilja använda sig av debug-utskrifter i Rust?

Att skriva ut till konsolen kan vara ett användbart verktyg för att se vad som händer i din kod i realtid. Det kan hjälpa dig att förstå hur olika värden ändras medan koden körs och kan vara till stor hjälp vid felsökning.

## Hur du gör

För att skriva ut debug-utskrifter i Rust behöver du använda dig av makron "println!" eller "dbg!". Dessa makron tar in variabler och värden som ska skrivas ut och skriver sedan ut dem till konsolen.

```Rust
let num = 42;
println!("Talet är: {}", num);
// Output: Talet är: 42

dbg!(num);
// Output: [src/main.rs:5] num = 42
```

Du kan även använda dig av "eprintln!" eller "edbg!" för att skriva till standard error istället för standard output.

## Djupdykning

I Rust finns det olika typer av utskriftsmakro, såsom "print!", "println!", "eprint!" och "eprintln!". Skillnaden mellan dem är att "println!" och "eprintln!" lägger till en radbrytning efter varje utskrift, medan "print!" och "eprint!" inte gör det.

Vidare, när du använder makron "dbg!" eller "edbg!", används makronet "format!" internt för att skapa den sträng som ska skrivas ut. Detta kan vara användbart om du vill skapa egna debug-utskrifter och behöver mer kontroll över formateringen.

## Se även

- [Rust dokumentation om debugging](https://doc.rust-lang.org/std/macro.dbg.html)
- [Debugging i Rust med Visual Studio Code](https://dev.to/creativcoder/debugging-rust-with-vs-code-3mjs)
- [Debugging i Rust med GDB](https://medium.com/@dguntur/code-debugging-in-rust-2f1c3a792f67)