---
title:                "Kontrollera om en katalog finns"
html_title:           "Bash: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en mapp existerar är en procedur inom programmering som säkerställer att en viss mapp finns i filsystemet. Programmerare gör detta för att undvika felsituationer som kan uppstå om mappen inte finns, till exempel 'FileNotFoundException'.

## Så här gör man:
Följande Gleam-kod visar hur man kontrollerar om en mapp existerar.

```Gleam
import gleam/otp/applications
import gleam/otp/application
import gleam/directory

fn check_directory() {
  let path = "/my/directory/path"
  let directory_exists = directory.exists(path)
  case directory_exists {
    Ok(exists) ->
      if exists {
        io.println("Mappen finns!")
      } else {
        io.println("Mappen finns inte!")
      }
      
    Error(err) ->
      io.println("Ett fel inträffade: ", err)
  }
}
```

Kör programmet och beroende på om mappen existerar, så kommer du att se antingen "Mappen finns!" eller "Mappen finns inte!".

## Djupdykning
Funktionen för att kontrollera om en mapp existerar i Gleam är relativt ny och faktiskt mycket lättare att använda jämfört med tidigare metoder. I äldre programmeringsmiljöer krävdes ibland ett helt bibliotek för att åstadkomma samma sak. 

Ett alternativ till att kontrollera om en mapp finns är att helt enkelt försöka använda mappen och hantera eventuella fel som uppstår. Men denna metod kan leda till onödigt komplex kod.

För att göra detta i Gleam, använder vi modulen `gleam/directory` och funktionen `exists`. Detta baseras på Erlangs inbyggda bibliotek för hantering av filsystemet, vilket ger oss stark driftsäkerhet och prestanda.

## Se även
För mer information om hantering av mappar och filer i Gleam, se de officiella dokumenten:

1. Gleam Directory Module: [https://hexdocs.pm/gleam_stdlib/gleam/directory](https://hexdocs.pm/gleam_stdlib/gleam/directory)
2. Gleam IO Module: [https://hexdocs.pm/gleam_stdlib/gleam/io](https://hexdocs.pm/gleam_stdlib/gleam/io)