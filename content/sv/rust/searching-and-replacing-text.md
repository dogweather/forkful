---
title:                "Rust: Sökning och ersättning av text"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

När man arbetar med programmering är det ofta nödvändigt att söka igenom en textsträng och ersätta vissa delar av den. Detta kan vara användbart för att snabbt göra förändringar i större mängder kod, för att uppdatera information eller för att lösa fel som uppstår. I Rust finns det effektiva och enkla sätt att söka och ersätta text, vilket kan underlätta arbetet och göra det mer effektivt.

## Så här gör man

För att söka och ersätta text i Rust använder man sig av standardbiblioteket "std::string::String" och dess metod "replace". Så här ser syntaxen ut:

```Rust
let mut text = String::from("Detta är en textsträng.");
let ny_text = text.replace("textsträng", "kodsnutt");
println!("Ny text: {}", ny_text;)
```

I detta exempel har vi en textsträng som vi vill byta ut ordet "textsträng" till "kodsnutt". Genom att använda "replace" metoden kan vi enkelt uppdatera textsträngen och se resultatet i konsolen. Det är också möjligt att ersätta flera förekomster av ett ord på en gång, genom att lägga till ett tredje argument i metoden som anger antalet bytes som ska ersättas.

## Djupdykning

En viktig sak att notera när man söker och ersätter text i Rust är att det är case-sensitive, vilket betyder att den tar hänsyn till skillnaden mellan stora och små bokstäver. Detta kan leda till problem om man vill ersätta ord i en text med blandade case. Man kan också använda sig av reguljära uttryck för att mer specifikt söka efter mönster i texten och ersätta det med önskad text. Det finns också alternativa metoder för sökning och ersättning beroende på vilken datastruktur som används, som till exempel "replace_all" för vektorer.

## Se också

- [Officiell Rust dokumentation för "replace" metoden](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [Guide för reguljära uttryck i Rust](https://doc.rust-lang.org/regex/regex/index.html)
- [Alternativa metoder för sökning och ersättning i Rust](https://docs.rs/str-replace/0.1.8/str_replace/)