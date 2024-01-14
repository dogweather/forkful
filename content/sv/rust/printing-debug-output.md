---
title:    "Rust: Utmatning av felsökningsinformation"
keywords: ["Rust"]
---

{{< edit_this_page >}}

#Varför man ska använda sig av utskrift av felmeddelanden i Rust
När man arbetar med programmering, oavsett språk, är det nästan garanterat att man kommer att stöta på fel och buggar. Dessa fel kan vara väldigt frustrerande och tidskrävande att hitta och lösa. En av de mest användbara verktygen för att hitta och felsöka dessa problem är att använda sig av utskrift av felmeddelanden, eller "debug output". I denna bloggpost kommer vi att utforska hur man kan använda sig av utskrift av felmeddelanden i Rust för att underlätta felsökning och förbättra kvaliteten på sin kod.

##Hur man använder sig av utskrift av felmeddelanden i Rust
I Rust finns det flera olika sätt att skriva ut felmeddelanden, men det vanligaste är att använda sig av makron `println!` och `eprintln!`. Dessa makron tar en sträng och eventuella variabler som argument och skriver ut det på standard output respektive standard error. Här är ett exempel på hur man kan använda dem:

```Rust
let name = "Alice";

println!("Hej, mitt namn är {}", name);
eprintln!("Det här är ett felmeddelande");
```

Output:

```
Hej, mitt namn är Alice

Det här är ett felmeddelande
```

Detta kan vara användbart för att kontrollera och felsöka variabler och värden i ens program under körning.

##Djupdykning i utskrift av felmeddelanden i Rust
En annan användbar funktion för utskrift av felmeddelanden i Rust är `dbg!` makron, vilket står för "debugger". Denna makro tar en variabel som argument och skriver ut både värdet och namnet på variabeln. Detta kan vara mycket användbart när man behöver ha mer detaljerad information om variabler och värden vid felsökning. Här är ett exempel:

```Rust
let a = 5;
let b = 3;

dbg!(a + b);
```

Output:

```
[a + b: 8]
```

En annan viktig aspekt av utskrift av felmeddelanden i Rust är möjligheten att ange vilken typ av information som ska skrivas ut, såsom till exempel filnamn och radnummer där utskriften sker. Detta kan hjälpa till att lokalisera och hitta problemet snabbare. Det finns också olika nivåer av utskrift, som kan ändras beroende på om man kör sitt program i utvecklings- eller produktionsmiljö.

##Se även
- Rusts officiella dokumentation om utskrift av felmeddelanden: https://doc.rust-lang.org/book/ch09-01-unrecoverable-errors-with-panic.html
- En tutorial om hur man använder `dbg!` i Rust: https://codetuts.tech/rust-debug/
- En guide för att välja rätt utskriftsnivå i Rust: https://kerkour.com/blog/rust-log-levels/