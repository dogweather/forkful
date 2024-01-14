---
title:                "Rust: Att påbörja ett nytt projekt"
programming_language: "Rust"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Varför

Har du någonsin velat lära dig ett nytt programmeringsspråk som är effektivt, snabbt och säkert? Då ska du ge Rust en chans! Det här moderna språket gör det möjligt att bygga pålitliga och robusta system, och det är också lättare att lära sig än du kanske tror.

## Hur man gör

För att komma igång med Rust bör du först installera Rust-kompilatorn och dess förpackningshanterare, Cargo. Sedan kan du följa dessa steg för att skapa ditt första Rust-projekt:

1. Öppna en terminal och navigera till mappen där du vill skapa ditt projekt.
2. Skriv kommandot `cargo new <projektnamn>` och tryck på Enter.
3. Navigera in i den nya mappen som skapades och öppna filen `src/main.rs` i en textredigerare.

Nu är det dags att börja koda i Rust! Här är ett exempel på hur du kan skriva en funktion som beräknar det tredje Fibonacci-talet:

```Rust
fn fib(n: u32) -> u32 {
    if n <= 1 {
        return n;
    } else {
        return fib(n-1) + fib(n-2);
    }
}

fn main() {
    let result = fib(3);
    println!("Det tredje Fibonacci-talet är {}", result);
}
```

Om du kör detta program kommer du att få utskriften `Det tredje Fibonacci-talet är 2`, eftersom det tredje talet i Fibonacci-serien är 2.

## Djupdykning

Nu när du har börjat bekanta dig med grundläggande syntax och funktioner i Rust är det dags att göra en djupdykning i språket. En av de viktigaste aspekterna i Rust är dess säkerhet, som uppnås genom att använda lånekontroller och äganderätt. Detta innebär att Rusts kompilator kontrollerar ditt program för att se till att det inte finns några fel som kan orsaka minnesläckor eller andra potentiellt farliga situationer.

Ett annat sätt att utforska Rust är att läsa och analysera öppen källkod som är skriven i språket. Genom att titta på hur andra har implementerat sina projekt kan du få en bättre förståelse för hur man använder olika koncept och tekniker i praktiken.

## Se även

- [Officiell Rust-webbplats](https://www.rust-lang.org/sv/)
- [Rust-programmering för nybörjare](https://doc.rust-lang.org/book/)
- [Rust dokumentation om lånekontroller och äganderätt](https://doc.rust-lang.org/book/ch04-01-what-is-ownership.html)
- [Öppen källkod i Rust på GitHub](https://github.com/rust-lang/rust)