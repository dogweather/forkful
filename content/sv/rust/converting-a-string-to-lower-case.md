---
title:                "Rust: Omvandla en sträng till små bokstäver"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

I många programmeringsprojekt har man behov av att konvertera en sträng till små bokstäver. Det kan vara för att göra sökningar eller jämförelser mer enhetliga eller för att skapa en enhetlig utdata. I denna artikel kommer vi att titta på hur man enkelt kan konvertera en sträng till små bokstäver i programmeringsspråket Rust.

## Hur man gör det

För att konvertera en sträng till små bokstäver i Rust använder man funktionen `to_lowercase()`. Detta är en inbyggd funktion i standardbiblioteket som kan användas på en sträng för att returnera en ny sträng med alla bokstäver i små bokstäver. Nedan följer ett exempel på hur man kan använda funktionen:

```Rust
let string = String::from("DET HÄR ÄR EN STRÄNG MED STORA BOKSTÄVER");
let new_string = string.to_lowercase();

println!("{}", new_string);

// Output: det här är en sträng med stora bokstäver
```

Funktionen `to_lowercase()` tar emot en referens till en sträng och returnerar en ny sträng i små bokstäver. Detta betyder att den ursprungliga strängen förblir oförändrad och en ny sträng skapas med de konverterade bokstäverna.

## Djupdykning

När man använder funktionen `to_lowercase()` är det viktigt att tänka på att den endast konverterar bokstäver som finns i ASCII-tabellen. Detta betyder att specialtecken eller bokstäver från andra språk kan behöva konverteras på andra sätt. Det är också viktigt att notera att konverteringen kan påverka prestandan i vissa fall, eftersom det kan innebära att en ny sträng måste skapas. Därför kan det vara bra att utföra tester på olika implementeringar för att hitta den som är mest effektiv för specifika syften.

## Se också

- [Rust dokumentation om `to_lowercase()`](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)
- [En diskussion om konvertering av specialtecken i Rust](https://stackoverflow.com/questions/31760928/how-can-i-convert-non-ascii-characters-from-a-string-to-their-ascii-counterpar)
- [En benchmarking av olika metoder för att konvertera strängar i Rust](https://users.rust-lang.org/t/speeding-up-string-conversions-lower-upper-case/12135)