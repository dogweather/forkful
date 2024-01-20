---
title:                "Omvandla en sträng till gemener"
html_title:           "Arduino: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad och Varför?

Att konvertera en sträng till gemener involverar att ändra alla versaler i en sträng till gemener. Detta är vanligt när programmerare vill jämföra strängar utan att ta hänsyn till skiftläge.

## Hur man gör:

I Rust kan du enkelt konvertera en sträng till gemener med `to_lowercase` funktionen. Här är ett exempel:

```Rust
fn main() {
    let s = "HEJ, VÄRLDEN!";
    let lower_s = s.to_lowercase();
    println!("{}", lower_s);
}
```

När du kör ovanstående program får du följande utdata:

```Rust
hej, världen!
```

## Djup Dykning:

För det första, `to_lowercase` är inget nytt: det har funnits i programmeringsspråk sedan början för att hantera skillnader i skiftläge.

Sommiga språk, som Python och JavaScript, har också liknande funktioner. Det finns dock inga direkta alternativ till `to_lowercase` i Rust. Funktionen använder Unicode Character Database för att korrekt konvertera strängar, även de med icke-engelska tecken.

Dessutom, `to_lowercase` skapar en ny `String` som resultatet. Originalsträngen är oförändrad. Detta är viktigt i Rust eftersom det är ett systemspråk som värderar minneshantering.

## Se Även:

För mer information om `to_lowercase` och andra stränghanteringsfunktioner i Rust, besök den officiella dokumentationen [här](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase). För att lära dig mer om strängjämförelser och skiftlägeskänslighet, kolla in denna [artikel](https://www.ietf.org/rfc/rfc3454.txt).