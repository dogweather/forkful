---
date: 2024-01-26 00:59:08.160488-07:00
description: "Felhantering handlar om att hantera saker n\xE4r de inte g\xE5r som\
  \ planerat. Programmerare g\xF6r det f\xF6r att hantera det ov\xE4ntade och s\xE4\
  kerst\xE4lla att deras Rust-\u2026"
lastmod: '2024-03-13T22:44:37.706250-06:00'
model: gpt-4-1106-preview
summary: "Felhantering handlar om att hantera saker n\xE4r de inte g\xE5r som planerat.\
  \ Programmerare g\xF6r det f\xF6r att hantera det ov\xE4ntade och s\xE4kerst\xE4\
  lla att deras Rust-\u2026"
title: Hantering av fel
---

{{< edit_this_page >}}

## Vad & Varför?

Felhantering handlar om att hantera saker när de inte går som planerat. Programmerare gör det för att hantera det oväntade och säkerställa att deras Rust-program är robusta och inte bara kraschar när de stöter på ett problem.

## Hur gör man?

Rust hanterar fel på två stora sätt: återhämtande och oåterhämtande fel. Låt oss titta på båda.

Återhämtande fel använder `Result<T, E>`:

```Rust
use std::fs::File;

fn open_file(filename: &str) -> Result<File, std::io::Error> {
    let f = File::open(filename);
    
    match f {
        Ok(file) => Ok(file),
        Err(e) => Err(e),
    }
}

fn main() {
    match open_file("hello.txt") {
        Ok(_file) => println!("Filen öppnades framgångsrikt."),
        Err(_e) => println!("Misslyckades med att öppna filen."),
    }
}
```

Utskriften kan bli antingen "Filen öppnades framgångsrikt." eller "Misslyckades med att öppna filen." beroende på din `hello.txt`.

För oåterhämtande fel, använder vi `panic!`:

```Rust
fn main() {
    // Detta kommer att orsaka att programmet får panik eftersom filen förmodligen inte finns.
    let _f = File::open("nowhere.txt").unwrap();
}
```

Kör det, och du kommer att se ett panikmeddelande. Ditt program stoppas direkt.

## Fördjupning

Historiskt sett har felhantering i programmering varit en röra. Rust gör det rätt med en tydlig skillnad mellan återhämtande och oåterhämtande fel.

Enumet `Result` är för återhämtande fel. Det är uttryckligt - du hanterar varianten `Ok` eller `Err`. Du har metoder som `unwrap()` och `expect()` också, men de är snabba och smutsiga genvägar som kan leda till en `panic!`.

`panic!` är Rusts sätt att skrika ut att något verkligen dåligt har hänt, och den kan inte hantera det. Det är som ett oåterhämtande fel som stoppar exekveringen omedelbart. En panik i Rust känns ofta i samband med buggar som du inte förväntar dig att hantera, som att indexera utanför arraygränser.

Felhantering genom att returnera `Result` är att föredra när du förväntar dig att hantera fel. Det är idiomatic Rust, vilket betyder att det är det sätt som Rust-utvecklare har kommit överens om att göra saker på. Det finns också `Option<T>`, för fall där ett fel bara är att något är `None` istället för `Some(T)`. Det handlar om att förvänta sig det oväntade utan rädsla.

Alternativ? Visst, du kan använda andra felhanteringspaket för fler funktioner eller ergonomisk användning. Som `anyhow` för enkel felhantering, eller `thiserror` för fel i bibliotekskod.

## Se även

Intresserad av att fördjupa dig? Här är vart du ska gå:

- [Rust Book om Felhantering](https://doc.rust-lang.org/book/ch09-00-error-handling.html) - En utmärkt plats för att förstå Rusts filosofi kring felhantering.
- [Rust genom Exempel: Felhantering](https://doc.rust-lang.org/rust-by-example/error.html) - Interaktiva exempel för att smutsa ner händerna.

Kom ihåg, bra felhantering är inte bara kodning; det är att ta hand om dina kodanvändare. Lycka till med kodningen!
