---
title:                "Kontrollera om en mapp finns"
html_title:           "Gleam: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Det är viktigt att kontrollera om en mapp existerar innan man försöker läsa från eller skriva till den. Detta säkerställer att ditt program inte kraschar på grund av en saknad mapp och det ger också möjlighet för dig att hantera eventuella felmeddelanden på ett bättre sätt.

## Så här gör du

Att kontrollera om en mapp existerar är enkelt med hjälp av Gleam's standardbibliotek. Du behöver bara använda funktionen `File.exists` och ge den sökvägen till mappen du vill kontrollera. Här är ett exempel:

```gleam
import gleam/io

let path = "./dokument/mapp"

let mapp_finns = io.File.exists(path)
```

Funktionen `File.exists` returnerar antingen `True` eller `False` beroende på om mappen finns eller inte. Om du vill kolla om en mapp INTE finns, kan du använda funktionen `File.not_exists`.

Du kan också använda `match` för att hantera olika fall beroende på resultatet av `File.exists`. Till exempel:

```gleam
import gleam/io

let path = "./dokument/mapp"

let mapp_finns = io.File.exists(path)

match mapp_finns {
    True -> io.print("Mappen finns!")
    False -> io.print("Mappen finns inte.")
}
```

## Deep Dive

Vad händer i bakgrunden när vi använder `File.exists`? Funktionen anropar faktiskt operativsystemets filsystem för att kontrollera om en fil eller mapp existerar. Om mappen inte existerar så returnerar operativsystemet ett felmeddelande som vårt program därefter behandlar.

Det finns också en annan funktion som du kan använda när du kontrollerar om en mapp existerar - `File.stat`. Denna funktion returnerar mer detaljerad information om en fil eller mapp, som till exempel storlek, skapandetid och ändringstid.

## Se även

Se Gleam's officiella dokumentation för mer information om `File.exists` och `File.stat`:

- [Officiell dokumentation för File.exists](https://gleam.run/documentation/#file.exists)
- [Officiell dokumentation för File.stat](https://gleam.run/documentation/#file.stat)