---
title:                "Omvandla ett datum till en sträng"
html_title:           "Rust: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Att konvertera ett datum till en sträng är en vanlig uppgift för programmerare. Det innebär att omvandla ett datumvärde till en läsbar textsträng, ofta i ett specifikt format. Detta är viktigt för att visa datumet i en användbar form för användare eller för att spara och skicka datum som data i en fil eller databas.

# Hur gör man?
I Rust finns ett inbyggt sätt att konvertera datum till strängar med hjälp av standardbiblioteket DateTime. Inkludera detta bibliotek genom att lägga till ```use std::time::DateTime``` i början av din kod. Sedan kan du konvertera ett datum till en sträng genom att använda DateTime-objektet och anropa dess ```format()``` -metod. Detta kräver två parametrar: ett datumformat och det datum som ska konverteras. Se nedan för kodexempel och tillhörande utdata.

```Rust 
use std::time::DateTime; 

let datum = DateTime::parse_from_rfc3339("2021-04-01T12:00:00+00:00").unwrap(); 
println!("Datumet som sträng: {}", datum.format("%d-%m-%Y")); 

```

Output: Datumet som sträng: 01-04-2021

# Djupdykning
Detta sätt att konvertera datum till strängar är baserat på en standard som kallas RFC 3339, vilken definierar ett format för datum och klockslag i Internetprotokoll. Detta är ett vanligt sätt att representera datum och används ofta i webbapplikationer eller API:er.

Alternativt kan programmerare använda en tredjepartsbibliotek som chrono för att konvertera datum till strängar. Detta bibliotek har fler funktioner och formatalternativ, men kräver också att det installeras och integreras i din kod.

När det gäller implementationen så konverterar metoden ```format()``` från DateTime-objektet datumet till UTC-tid och sedan till ett valt format. Detta ger en textsträng som kan användas på olika plattformar och enheter.

# Se även
För mer information om konvertering av datum till strängar i Rust, se dokumentationen för DateTime-modulen: https://doc.rust-lang.org/std/time/struct.SystemTime.html#method.format