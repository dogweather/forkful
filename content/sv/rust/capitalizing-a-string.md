---
title:                "Rust: Att Göra en Sträng StorBokstavlig"
simple_title:         "Att Göra en Sträng StorBokstavlig"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Varför skulle någon vilja ägna sig åt att göra bokstäver stora i en sträng? För det första kan det vara ett vanligt problem för utvecklare att behöva ändra storlek på bokstäver i en sträng för att få dem att passa samman med andra delar av koden. Med Rust-programmeringsspråket kan du enkelt lösa detta problem genom att använda inbyggda funktioner för att göra din kod mer effektiv och lättläst.

## Hur man gör

För att göra bokstäver stora i en sträng i Rust, kan du använda funktionen `to_uppercase()`. Detta är en inbyggd funktion som gör att alla bokstäver i en sträng blir stora. Se nedan för ett enkelt kodexempel:

```Rust
let string = "hej!";

let uppercase_string = string.to_uppercase();

println!("Strängen blir nu: {}", uppercase_string);
```

**Utmatning:**

`Strängen blir nu: HEJ!`

Som du kan se i exemplet ovan, använde vi funktionen `to_uppercase()` på en variabel som innehåller strängen "hej!". Funktionen returnerar en ny variabel med samma sträng, men alla bokstäver blir stora. Se till att hålla koll på returvärdet från funktionen eftersom den inte ändrar den ursprungliga strängen.

## Fördjupning

Det är viktigt att notera att funktionen `to_uppercase()` endast fungerar för ASCII-bokstäver, vilket betyder att det inte fungerar på andra språk eller specialtecken. Om du behöver hantera sådana tecken, kan du använda en externa bibliotek som `unicode-norm` eller `rust-i18n` för att få tillgång till funktioner som klarar av detta.

Dessutom kan du också använda funktionen `to_lowercase()` för att göra alla bokstäver små i en sträng. Detta kan vara användbart om du vill ha en enhetlig textformatering i din kod eller om du behöver göra jämförelser mellan strängar utan att bry dig om deras storlek.

## Se även

- [Rust´s dokumentation för strängar](https://doc.rust-lang.org/std/string/index.html)
- [Rust´s dokumentation för `to_uppercase()` och `to_lowercase()`](https://doc.rust-lang.org/std/string/struct.String.html#method.to_uppercase)
- [Unicode-norm biblioteket för hantering av icke-ASCII-tecken](https://crates.io/crates/unicode-normalization)