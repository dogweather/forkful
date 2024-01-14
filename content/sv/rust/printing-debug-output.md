---
title:    "Rust: Utskrift av felsökningsutdata"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva ut debug-utdata är ett viktigt steg i utvecklingsprocessen för att hitta och fixa buggar i din Rust-kod. Genom att utskrift debug-utdata kan du få en bättre förståelse av hur din kod fungerar och var eventuella fel kan uppstå.

## Så här gör du

För att skriva ut debug-utdata i Rust, används makron "println!" och "dbg!". Dessa tar in det du vill skriva ut och skriver sedan ut det till konsolen.

```Rust
fn main() {
   let num: u32 = 5;
   println!("Debug-utdata: {}", num); // Skriver ut "Debug-utdata: 5"
   dbg!(num); // Skriver ut "num: 5" och returnerar värdet
}
```

Du kan också skriva ut flera variabler samtidigt genom att separera dem med ett kommatecken inuti parantesen för "println!" och "dbg!".

```Rust
fn main() {
   let name = "Johan";
   let age = 26;
   println!("Namn: {}, Ålder: {}", name, age); // Skriver ut "Namn: Johan, Ålder: 26"
   dbg!(name, age); // Skriver ut "name: "Johan", age: 26" och returnerar sista värdet (åldern)
}
```

## Djupdykning

För att få ännu mer detaljerad debug-utdata, kan du använda "dbg!" med en conditional statement. Då kommer det bara att skrivas ut om conditionen är sann.

```Rust
fn main() {
   let num1 = 5;
   let num2 = 10;

   dbg!(num1, num2 == num1 * 2); // Skriver bara ut "num2 == num1 * 2" eftersom det är det enda som är sant
}
```

En annan användbar funktion är att använda "dbg!" tillsammans med "assert!" för att kontrollera om dina variabler har rätt värde vid en viss punkt i kodens användning. Om värdet inte är rätt, kommer det att sluta programmet med ett felmeddelande.

```Rust
fn main() {
   let age = 15;

   dbg!(age);
   assert!(age >= 18, "Åldern är inte rätt!"); // Om åldern är mindre än 18 kommer programmet att avslutas och skriva ut "Åldern är inte rätt!"
}
```

## Se också

- Rust dokumentation för [println!](https://doc.rust-lang.org/std/macro.println.html) and [dbg!](https://doc.rust-lang.org/std/macro.dbg.html)
-Rust programming bloggartikel: [Debugging med Rust](https://lindseybieda.com/rust/debugging-with-rust/)
- Rust video tutorial: [Print Debug Data Using println! and dbg! in Rust](https://www.youtube.com/watch?v=UVmugGMxOdk)