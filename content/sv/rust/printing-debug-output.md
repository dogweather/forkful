---
title:                "Utskrift av felsökningsresultat"
html_title:           "Rust: Utskrift av felsökningsresultat"
simple_title:         "Utskrift av felsökningsresultat"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & varför?
Att skriva ut debug-utdata är när man som programmerare skriver ut relevant och användbar information för att underlätta felsökning och förbättra kodens prestanda.

Det är vanligtvis användbart när man vill se vad som händer i ett program under körning och för att hitta eventuella buggar eller ineffektiv kod.

## Hur man gör det:

Här är ett enkelt exempel på hur man skriver ut en textsträng i Rust:

```rust
println!("Hej världen!");
```

Detta kommer att skriva ut "Hej världen!" i terminalen eller i konsolen när programmet körs.

Här är ett mer avancerat exempel som visar hur man kan använda formatmallar för att skriva ut variabler tillsammans med text:

```rust
let num1 = 5;
let num2 = 10;

println!("Summan av {} och {} är {}", num1, num2, num1 + num2);
```

Detta kommer att skriva ut "Summan av 5 och 10 är 15".

## Djupdykning:

Att skriva ut debug-utdata har funnits länge inom programmering och är en vanlig metod för att felsöka kod. Innan det fanns moderna debugging-verktyg var det vanligt att skriva ut information om variabler och programflöde för att hitta problem.

Ett alternativ till att skriva ut debug-utdata är att använda en debugger, en speciell typ av program som hjälper dig att spåra och fixa fel i din kod. Detta kan vara mer effektivt än att bara skriva ut information, men vissa föredrar fortfarande att använda printf-debugging då det ger en bättre överblick av koden.

I Rust finns det flera sätt att skriva ut debug-utdata. Det vanligaste är att använda makron som ```println!``` och ```eprintln!```, men det finns också andra alternativ som t.ex. ```dbg!``` och ```format!``` som ger mer kontroll över utdataformatet.

## Se även:

- [Rust bok](https://doc.rust-lang.org/book/ch05-01-defining-structs.html#debugging-with-println)
- [Rust RFC om debugging-makron](https://github.com/rust-lang/rfcs/blob/master/text/2329-custom-print-formatters.md)
- [Rust dokumentation om formatmakron](https://doc.rust-lang.org/std/fmt/#formatting-traits)