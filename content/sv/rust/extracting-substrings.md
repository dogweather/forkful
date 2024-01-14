---
title:    "Rust: Att extrahera delsträngar"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Substring-extrahering är en viktig del av programmering, och det kan vara användbart i många olika scenarier. Med hjälp av Rusts effektiva och korrekta syntax, kan man enkelt extrahera substrings från en given sträng.

## Hur man gör

För att extrahera substrings i Rust behöver du först importera `std::str::Chars`, som ger tillgång till strängens tecken. Sedan kan du använda `range`-syntaxen för att välja vilka tecken du vill extrahera.

```Rust
use std::str::Chars;

let string = "Hej! Välkommen";
let chars = string.chars(); // få tillgång till strängens tecken
let substring = chars.range(4..11); // välja tecken från position 4 till 11

println!("Din substring är: {}", substring); // output: Välkommen
```

Om du vill extrahera en del av strängen från ett specifikt tecken till slutet kan du använda `range_from`-syntaxen istället.

```Rust
use std::str::Chars;

let string = "Hej! Välkommen";
let chars = string.chars();
let substring = chars.range_from(4); // välja tecken från position 4 och framåt

println!("Din substring är: {}", substring); // output: Välkommen
```

Det är också möjligt att extrahera en del av strängen från början till en viss position genom att använda `range_to`-syntaxen.

```Rust
use std::str::Chars;

let string = "Hej! Välkommen";
let chars = string.chars();
let substring = chars.range_to(6); // välja tecken från början till position 6

println!("Din substring är: {}", substring); // output: Hej!
```

## Djupdykning

För att få tillgång till alla funktioner för att extrahera substrings i Rust, kan du titta på dokumentationen för `std::str::Chars`. Där kan du hitta fler exempel på hur man kan använda `range`-syntaxen för att välja delar av strängen.

Det är också värt att notera att `Chars`-modulen även har en `take`-funktion som gör det möjligt att välja ett specifikt antal tecken från början av strängen.

```Rust
use std::str::Chars;

let string = "Hej! Välkommen";
let chars = string.chars();
let substring = chars.take(4); // välja de första 4 tecknen från början

println!("Din substring är: {}", substring); // output: Hej!
```

## Se även

- [Rust officiell dokumentation för substrings](https://doc.rust-lang.org/std/primitive.str.html#method.char_indices)
- [Tutorial: Substring-extrahering i Rust](https://scastie.scala-lang.org/P98V5hNdTruSpo2RlAtF6w)
- [Exempel på hur man använder `range`-syntaxen för substring-extrahering](https://doc.rust-lang.org/rust-by-example/std/str.html#slicing)