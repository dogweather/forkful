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

## Vad & Varför?
Att kontrollera om en mapp existerar är en vanlig uppgift för programmerare. Det är en process som innebär att man undersöker om en mapp finns på en viss plats på en dator. Detta kan vara användbart för att se till att programmet kan hitta och använda nödvändiga resurser.

## Hur man gör:
För att kontrollera om en mapp existerar i Gleam, kan man använda sig av funktionen ```Gleam.fs.exists```. Nedan finns ett kodexempel som visar hur man kan implementera detta:

```
import gleam/fs

fn check_directory_exists(path) {
  case Gleam.fs.exists(path) {
    True -> "Mappen finns."
    False -> "Mappen finns inte."
  }
}
```

Om man till exempel vill kontrollera om en mapp med namnet "Dokument" finns i den aktuella mappen, kan man skriva:

```
fn run() {
  check_directory_exists("./Dokument")
}
```

Detta kommer att ge oss utskriften "Mappen finns." om mappen existerar, eller "Mappen finns inte." om den inte gör det.

## Djupdykning:
Det finns flera alternativ för att kontrollera om en mapp existerar, bland annat genom att använda sig av kommandot ```Gleam.os.path.exists``` för att specificera en absolut sökväg. Det finns också andra programmeringsspråk som erbjuder liknande funktioner, som till exempel Python med sin ```os.path.exists()```. I Gleam implementeras ```Gleam.fs.exists``` som en wrapper runt ```Gleam.os.path.exists```, vilket gör det möjligt att enkelt anpassa funktionen vid behov.

## Se också:
- [Gleam fs modul](https://gleam.run/documentation/stdlib/fs)
- [Python os modul](https://docs.python.org/3/library/os.path.html)