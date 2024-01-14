---
title:    "Rust: Skriva en textfil"
keywords: ["Rust"]
---

{{< edit_this_page >}}

##Varför

Att skriva en textfil är en grundläggande men viktig del av programmering. Det kan användas för att spara data, skapa loggar eller helt enkelt hålla ordning på information.

##Så här gör du

Att skriva en textfil i Rust är enkelt. Först måste vi importera File-modulen och File-objektet, som tillhandahålls av standardbiblioteket.

```Rust
use std::fs::File;
use std::io::prelude::*;
```

Sedan skapar vi en ny fil med önskat namn och önskat läge (till exempel "skrivfil.txt") och öppnar den för skrivning.

```Rust
let mut fil = File::create("skrivfil.txt").unwrap();
```

Nu kan vi skriva till filen med hjälp av `write_all`-funktionen, som tar in en skrivbar `&[u8]`-referens som argument.

```Rust
fil.write_all(b"Hej från den här filen!").unwrap();
```

Till sist stänger vi filen för att slutföra skrivningen.

```Rust
fil.sync_all().unwrap();
```

Och voilà, nu har vi en textfil med vår sträng som innehåll!

##Djupdykning

När vi skriver till en fil i Rust används en process som kallas "I/O streaming". Det betyder att data strömmar från vår källa (i detta fall vår `&[u8]`-referens) och skrivs till filen bit för bit. Detta är en effektiv och säker metod som garanterar att all data sparas korrekt.

En annan viktig aspekt att notera är att filen måste stängas efter att skrivningen är klar, annars kan det leda till oförutsett beteende eller till och med dataförlust.

##Se även

- [Rust standardbibliotekets dokumentation för File-modulen](https://doc.rust-lang.org/std/fs/struct.File.html)
- [En guide till filhantering i Rust av The Rust Programming Language Book](https://doc.rust-lang.org/book/ch12-00-an-io-project.html)