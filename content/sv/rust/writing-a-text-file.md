---
title:                "Skriva en textfil"
html_title:           "Rust: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att skriva en textfil kan vara användbart för att spara information eller för att dela med andra. Det är också ett enkelt sätt att öva på din programmeringskunskaper i Rust.

## Så här gör du

För att skriva en textfil i Rust behöver du först importera en modul som heter "fs". Sedan kan du använda funktionen "write" för att skapa en ny fil och skriva in den önskade texten. Se till att sätta rätt filnamn och sökväg när du skapar filen. Här är ett exempel på hur det kan se ut:

```Rust
use std::fs::File;

fn main() {
    let mut fil = File::öppna ("min_textfil.txt", "V"). förvänta_sig("Kunde inte öppna filen");

    filskriva_all("Hej, det här är en textfil skriven i Rust!"). förvänta_sig("Kunde inte skriva till filen");
}

```

När du kör koden ovan kommer det att skapas en fil med namnet "min_textfil.txt" i samma mapp som ditt Rust-program. Inuti textfilen kommer texten "Hej, det här är en textfil skriven i Rust!" att finnas.

Du kan också använda funktionen "write_all" för att skriva flera rader text till filen. Se till att lägga till "\ n" mellan varje rad så att de skrivs på separata rader.

```Rust
use std::fs::File;

fn main() {
    let mut fil = File::öppna ("min_textfil.txt", "V"). förvänta_sig("Kunde inte öppna filen");

    filskriva_all("Det här är rad ett!\nDet här är rad två!"). förvänta_sig("Kunde inte skriva till filen");
}
```

## Djupdykning

När du skriver en textfil i Rust finns det några viktiga saker att komma ihåg. Först och främst måste du använda "mut" när du deklarerar filvariabeln, eftersom vi kommer att ändra filen när vi skriver till den. Det är också viktigt att sätta rätt rättighetstyper ("V" för skrivåtkomst) när du öppnar filen, annars kommer inte koden att fungera. Om du vill läsa från en befintlig fil kan du använda funktionen "read_to_string" istället för "write_all".

Det är också viktigt att hantera eventuella fel som kan uppstå under skrivprocessen. I våra exempel använde vi funktionen "förvänta sig", vilket innebär att om det uppstår ett fel kommer programmet att avslutas och felmeddelandet visas. Men du kan också välja att hantera felet på annat sätt, till exempel med en "match" -sats eller en "if-else" -sats.

## Se också

[Filhantering i Rust](https://doc.rust-lang.org/book/ch12-00-an-io-project.html) 
[Rust-fs-modulen](https://doc.rust-lang.org/std/fs/)