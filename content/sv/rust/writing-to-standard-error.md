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

## Varför

Att skriva till standard error kan vara användbart för att skriva ut felmeddelanden eller andra viktiga meddelanden som inte behöver visas för användaren. Det kan hjälpa till att felsöka och förbättra programmet.

## Hur man gör det

Det finns flera sätt att skriva till standard error i Rust, men det enklaste sättet är att använda "eprint!" eller "eprintln!" makron. Här är ett exempel på hur man skriver ut ett felmeddelande till standard error:

```Rust
let error_message = "Något gick fel!";
eprintln!("Fel: {}", error_message);
```

Detta kommer att skriva ut "Fel: Något gick fel!" till standard error. Notera att användningen av "eprintln!" makronet kräver att du importerar "std::io::Write" biblioteket.

En annan metod är att använda "writeln!" makronet och ange standard error som första argument. Detta gör att du inte behöver importera "std::io::Write" biblioteket. Här är ett exempel på hur man skriver till standard error med "writeln!" makronet:

```Rust
writeln!(std::io::stderr(), "Något gick fel!");
```

Det finns också en "stderr!" funktion som kan användas för att skriva till standard error. Här är ett exempel på hur man kan använda den:

```Rust
use std::io::{self, Write};

if let Err(e) = do_something_that_may_fail() {
    let mut stderr = io::stderr();
    let _ = writeln!(&mut stderr, "Error: {}", e);
}
```

Notera att det kan finnas andra sätt att skriva till standard error, men dessa tre är de vanligaste och enklaste att använda.

## Djupdykning

När du skriver till standard error används standard error begränsade buffert, vilket innebär att skrivning till det är relativt snabbt. Det är dock viktigt att notera att det inte är avsett för att skriva stora mängder data, det är mer för utskrift av felmeddelanden och andra viktiga meddelanden.

Det finns också möjlighet att använda andra bibliotek för att hantera standard error, som "stderrlog" som ger mer flexibilitet och kontroll över hur meddelanden skrivs till standard error.

## Se även

- [Rust standardbiblioteket](https://doc.rust-lang.org/std/index.html)
- [Rust dokumentation om hantering av standard streams](https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html#managing-growth-by-separating-error-handling-logic)