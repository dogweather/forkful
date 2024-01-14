---
title:    "Rust: Skrivning till standardfel"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error i Rust kan hjälpa dig att identifiera och hantera fel i ditt program. Genom att skriva felmeddelanden till standard error, istället för standard output, kan du enklare felsöka och förstå vad som går fel i ditt program.

## Hur man gör det

För att skriva till standard error i Rust, behöver du först importera standardbiblioteket "std" och sedan använda funktionen "eprint!" eller "eprintln!" beroende på om du vill skriva på samma rad eller på en ny rad.

```Rust
use std::io::{self, Write};
// använd "eprint!" för att skriva på samma rad
eprint!("Ett fel har inträffat");
io::stdout().flush().unwrap();
// använd "eprintln!" för att skriva på en ny rad
eprintln!("Ett fel har inträffat");
```

När du skriver till standard error, är det också viktigt att se till att den ordinarie output också skickas till standard error. Om du använder en print-funktion eller en loggningssamling såsom "log" måste du se till att även ditt felsökningsmeddelande skickas till standard error.

## Djupdykning

Att skriva till standard error kan vara särskilt användbart när du utvecklar större system med flera olika komponenter. Genom att skriva till standard error, kan du få en tydligare bild av vilka moduler och funktioner som är inblandade i felaktig kod, vilket gör felsökningen mer effektiv.

En annan fördel med att skriva till standard error är att det är en standardiserad metod för att hantera felmeddelanden. Det gör det enkelt för andra utvecklare att förstå källan till ett fel och hjälper till att skapa en bättre kodstruktur i ett projekt.

## Se också

- [Rust dokumentation om standardbiblioteket](https://doc.rust-lang.org/std/io/index.html)
- [Exempel på hur man skriver till standard error i Rust](https://stackoverflow.com/questions/53460024/how-to-write-to-stderr-in-rust)
- ["10 praktiska tips för att hantera fel i Rust"](https://blog.logrocket.com/10-practical-tips-for-handling-errors-in-rust/)