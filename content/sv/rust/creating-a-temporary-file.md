---
title:                "Att skapa en tillfällig fil"
html_title:           "Bash: Att skapa en tillfällig fil"
simple_title:         "Att skapa en tillfällig fil"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & varför?
Att skapa en tillfällig fil innebär att man tillfälligt lagrar data på disken istället för i programminnet. Det är användbart när du hanterar stora datamängder, vill dela data mellan olika programprocesser eller när du behöver överleva programkrascher.

## Hur man gör:
Här är en exempelkod av hur man skapar en tillfällig fil i Rust:

```Rust
use std::fs::File;
use std::io::Write;
use tempfile::tempfile;

fn main() {
    let mut file = tempfile().unwrap();
    
    write!(file, "Hej världen").unwrap();
}
```

När du kör detta kommer "Hej världen" att skrivas till en tillfällig fil. Filen är automatiskt borttagen efter att `file` faller ur räckvidd (dvs. när programmet är klart).

## Djupdykning
Tillfälliga filer har använts sedan de tidiga dagarna av programmering, då minnesresurser var begränsade. I Rust erbjuder `tempfile`-biblioteket ett säkert och bekvämt sätt att hantera tillfälliga filer.

Alternativen till att skapa en tillfällig fil innefattar att skapa vanliga filer och sedan manuellt ta bort dem. Men detta tillvägagångssätt kräver mer kod och kan eventuellt lämna kvar filer om programmet kraschar.

När du skapar en tillfällig fil med `tempfile()`, skapas den faktiskt i systemets standardkatalog för tillfälliga filer - vanligtvis `/tmp` på Unix-liknande system eller `C:\Windows\Temp` på Windows.

## Se även
Kolla in dessa resurser om du vill lära dig mer:
- Rusts officiella dokumentation på tempfile: https://docs.rs/tempfile
- Råd för filhantering i Rust: https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html