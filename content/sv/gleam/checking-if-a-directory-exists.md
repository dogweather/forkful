---
title:                "Kontrollera om en katalog finns"
date:                  2024-01-20T14:56:37.727310-07:00
html_title:           "Fish Shell: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kolla om en katalog finns innebär att du kontrollerar att en viss sökväg pekar på en riktig katalog i filsystemet. Det görs för att försäkra sig om att filoperationer såsom läsa och skriva kan utföras utan fel.

## Hur gör man:
I Gleam kan en enkel funktion för att kontrollera om en katalog finns se ut så här:

```gleam
import gleam/io

pub fn check_directory_exists(path: String) -> Result(Bool, Nil) {
  io.is_dir(path)
}

pub fn main() {
  let path = "path/to/your/directory"
  let does_exist = check_directory_exists(path)
  
  case does_exist {
    Ok(True) -> io.println("Katalogen finns!")
    Ok(False) -> io.println("Katalogen finns inte.")
    Error(_) -> io.println("Kunde inte kontrollera katalogen.")
  }
}
```
Förväntat utskriftsexempel beroende på katalogens status:
```
Katalogen finns!
```
eller,
```
Katalogen finns inte.
```

## Djupdykning
Historiskt sätt har kontroll för om kataloger och filer finns länge varit en del av många programmeringsspråk och deras standardbibliotek. I Gleam, som är byggt ovanpå Erlang VM (BEAM), utförs filsystem-operationer genom att kalla på underliggande Erlang-funktioner. Alternativ för att kontrollera om en katalog finns inkluderar att direkt använda Erlang-bibliotek eller att använda andra Gleam-paket som hanterar filsystem-operationer. Implementationsmässigt är det viktigt att hantera fel som kan uppstå när filsystemets status eller tillgänglighet är osäker.

## Se även
- Erlang :file-modulen dokumentation för jämförelse: [https://erlang.org/doc/man/file.html](https://erlang.org/doc/man/file.html)
