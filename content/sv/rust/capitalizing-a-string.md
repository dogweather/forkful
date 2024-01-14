---
title:    "Rust: Konvertera en sträng till versaler"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna konvertera en sträng till versaler är en viktig funktion inom programmering. Till exempel kan det vara användbart när man vill jämföra två strängar utan att bry sig om storleken på bokstäverna. I den här bloggposten kommer vi att lära oss hur man kapitaliserar en sträng i programmeringsspråket Rust.

## Hur man gör

För att kapitalisera en sträng i Rust finns det ett par olika sätt att göra det på. Ett enkelt sätt är att använda standardbiblioteket och dess `to_uppercase()`-funktion. Se nedan för ett exempel:

```Rust
let str = "hej!";
let capitalized_str = str.to_uppercase();

println!("{}", capitalized_str); // HEJ!
```

En annan metod är att använda `String`-typen i Rust och funktionen `chars()` för att iterera över varje tecken i strängen och konvertera dem individuellt till versaler. Se nedan för ett annat exempel:

```Rust
let str = "hej!";
let mut capitalized_str = String::new();

for c in str.chars() {
    capitalized_str.push(c.to_uppercase().next().unwrap());
}

println!("{}", capitalized_str); // HEJ!
```

Det finns också möjlighet att använda en tredje parts bibliotek, till exempel `strum` eller `regex`, för att få mer avancerade möjligheter för att konvertera strängar till versaler.

## Djupdykning

Att konvertera en sträng till versaler kan verka som en enkel uppgift, men det finns faktiskt en hel del logik bakom det. Till exempel måste man ta hänsyn till specialtecken och diakritiska tecken. Dessutom kan skillnaden mellan ASCII och Unicode påverka hur konverteringen görs. Därför är det viktigt att förstå de olika metoderna och deras begränsningar när man jobbar med strängar i Rust.

## Se även

För mer information om strängoperationer i Rust, se nedan för några användbara länkar:

- [Rust Standardbiblioteket - Strängar](https://doc.rust-lang.org/std/string/)
- [Rust Dokumentation - strum](https://docs.rs/strum/0.18.0/strum/)
- [Regex Modulen i Rust](https://docs.rs/regex/1.4.2/regex/)