---
title:                "Rust: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa en textfil kan verka som en enkel uppgift, men med Rusts kraftfulla funktioner och prestanda kan det leda till bättre och snabbare program. Läs vidare för att lära dig mer om hur du kan använda Rust för att läsa och hantera textfiler.

## Så här gör du

För att läsa en textfil i Rust behöver du först skapa en "File" variabel som är kopplad till den specifika textfilen. Detta kan göras med hjälp av "File::open()" funktionen.

```Rust
let file = File::open("textfil.txt");
```

Nästa steg är att skapa en "BufReader" som hjälper till att läsa filen rad för rad. För att göra detta använder vi "BufReader::new()" funktionen och förser den med vår "File" variabel.

```Rust
let reader = BufReader::new(file);
```

Nu kan vi använda en "for-loop" för att läsa igenom varje rad i filen och skriva ut den på skärmen.

```Rust
for line in reader.lines() {
    let line = line.unwrap();
    println!("{}", line);
}
```

## Djupdykning

En av de största fördelarna med att läsa en textfil i Rust är möjligheten att läsa och hantera filen på ett säkert och effektivt sätt. Med hjälp av Rusts "match" uttryck och "Result" typ kan vi kontrollera för eventuella fel under läsprocessen och hantera dem på ett strukturerat sätt.

Det är också möjligt att läsa enbart en del av en textfil istället för hela filen. Detta kan utföras med hjälp av "seek" funktionen som flyttar läspositionen till en specifik punkt i filen.

Genom att kombinera dessa tekniker kan du skriva robusta och effektiva program för att läsa och hantera textfiler.

## Se även

- [Officiell Rust dokumentation för att läsa filer](https://doc.rust-lang.org/std/fs/struct.File.html)
- [Rust By Example - Filhantering](https://doc.rust-lang.org/stable/rust-by-example/std_misc/file.html)
- [Läs filer med BufReader i Rust](https://levelup.gitconnected.com/reading-files-line-by-line-in-rust-17d1c2a0b8e7)