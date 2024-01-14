---
title:                "Rust: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför
Att konvertera en sträng till små bokstäver är en vanlig operation inom textbehandling och kan vara användbart i många olika situationer. Genom att lära dig att göra detta i Rust kan du utöka din kunskap om språket och använda det i dina egna projekt.

## Hur man gör det
För att konvertera en sträng till små bokstäver i Rust, kan du använda funktionen `to_lowercase()` från standardbiblioteket. Här är ett enkelt exempel:

```Rust
let original_str = "Hej, SVERIGE!";
let lowercase_str = original_str.to_lowercase();
println!("{}", lowercase_str);
```

Output:

```bash
hej, sverige!
```

Som du kan se, så har funktionen `to_lowercase()` omvandlat alla bokstäver i den ursprungliga strängen till små bokstäver.

Det är också värt att nämna att denna funktion inte endast fungerar för ASCII-tecken, utan även för andra språk som stöds av Unicode.

## Djupdykning
Det finns flera olika sätt att konvertera en sträng till små bokstäver i Rust. Förutom `to_lowercase()` funktionen, kan du också använda standardbiblioteket `to_ascii_lowercase()` eller implementera din egen algoritm för detta ändamål.

Det är också viktigt att komma ihåg att konverteringen till små bokstäver kan påverka prestandan i din kod. Om du behöver köra dina strängar igenom denna operation många gånger, så kan det vara värt att titta på alternativa lösningar för att inte påverka prestandan negativt.

## Se även
- The Rust Programming Language Book: Strings
- Rust Standard Library: `to_lowercase()`
- Rust Standard Library: `to_ascii_lowercase()`