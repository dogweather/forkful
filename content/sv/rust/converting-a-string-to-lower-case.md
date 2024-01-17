---
title:                "Omvandla en sträng till gemener"
html_title:           "Rust: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera en sträng till gemener innebär att göra alla bokstäver i strängen små bokstäver istället för stora. Detta är en vanlig operation inom programmering för att göra jämförelser av strängar case-insensitiva, vilket betyder att stora och små bokstäver behandlas som samma.

## Hur man gör:
Ett enkelt sätt att göra detta i Rust är att använda funktionen `to_lowercase()`, som är en del av standardbiblioteket. Här är ett exempel på hur man kan använda den:

```Rust
let sträng = "Hej Värld!";
let lowercase_sträng = sträng.to_lowercase();
println!("{}", lowercase_sträng);
```

Resultatet blir "hej värld!".

En annan möjlighet är att använda `chars()` funktionen för att iterera över varje tecken i strängen och sedan använda `to_lowercase()` på varje enskilt tecken. Detta kan vara användbart om man vill göra andra operationer på strängen samtidigt. Här är ett exempel:

```Rust
let mut sträng = String::from("Hej Värld!");
for c in sträng.chars() {
    let lowercase_c = c.to_lowercase().to_string();
    // Gör något med det lowercase tecknet här
}
```

## Djupintervju:
Att konvertera en sträng till gemener har varit en viktig del av programmering sedan tidiga datorer. I Unicode-standarderna behandlas gemener och versaler som olika tecken, vilket innebär att konverteringen av bokstäver är mer komplicerad än bara att göra dem små.

Det finns flera alternativ för att konvertera en sträng till gemener i Rust, inklusive `make_ascii_lowercase()` och `make_ascii_lowercase_inplace()` som enbart fungerar på ASCII-tecken och är därför snabbare. Det finns också externa bibliotek som kan användas för att göra konverteringen eller för att göra den snabbare för stora mängder data.

Implementeringen av `to_lowercase()` i standardbiblioteket använder Unicode-standardernas regler för att identifiera tecken som kan ha en gemensam gemena form, och sedan konverterar dem enligt dessa regler. Det är också möjligt att ange ett språkkodargument till funktionen för att använda språkspecifika regler för konverteringen.

## Se även:
För mer information om olika sätt att hantera strängar i Rust, se "The Rust Programming Language" bokens kapitel om strängar (https://doc.rust-lang.org/book/ch08-02-strings.html). För en djupare förståelse av Unicode-standarderna, se Unicode-konsortiets hemsida (https://unicode.org/).