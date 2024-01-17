---
title:                "Skriva till standardfel"
html_title:           "Rust: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Att skriva till standardfel är en vanlig praxis inom programmering för att informera om eventuella problem eller fel som kan uppstå under körning av ett program. Detta ger utvecklare en möjlighet att fånga och hantera dessa fel för att förbättra kvaliteten på sin kod.

# Hur man gör:
Kodexempel:

```Rust
use std::io::Write; // Importerar modulen för att möjliggöra skrivning till standardfel

fn main() {
    let msg = "Ett fel har inträffat!"; // Ett meddelande som ska skrivas till standardfel
    let stderr = std::io::stderr(); // Hämtar en referens till standardfel
    
    writeln!(&mut stderr.lock(), "{}", msg).unwrap(); // Skriver meddelandet till standardfel
}
```

Exempeloutput (i standardfel):

```
Ett fel har inträffat!
```

# Djupdykning:
Skrivning till standardfel har funnits i många år och är ett standardiserat sätt att hantera fel i många olika programmeringsspråk. I vissa fall kan alternativ som att logga fel till en fil eller konsol användas istället, men skrivning till standardfel är den rekommenderade metoden för att kommunicera fel till utvecklare.

För implementationen av att skriva till standardfel, används en modul som kallas "std::io::stderr". Denna modul möjliggör skrivning till standardfel genom att tillhandahålla en referens till standardfel och dess låsning. Genom att använda Rusts makrospråk kan vi sedan formatera ett meddelande och skriva det till standardfel utan att behöva hantera felhantering själva.

# Se även:
- [Rust dokumentation om standardfel](https://doc.rust-lang.org/std/io/struct.Stderr.html)
- [En guide till hur man skriver till standardfel i Rust](https://www.joshmcguigan.com/blog/redirecting-error-stream-rust/)