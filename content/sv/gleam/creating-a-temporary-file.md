---
title:                "Att skapa en tillfällig fil"
html_title:           "Gleam: Att skapa en tillfällig fil"
simple_title:         "Att skapa en tillfällig fil"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Skapande av en tillfällig fil är en vanlig uppgift för programmerare som behöver manipulera data eller spara temporära resultat. En temporär fil är en fil som skapas tillfälligt och sedan raderas efter att den har använts.

# Hur?
För att skapa en temporär fil i Gleam använder du funktionen `File.temp` och anger en prefix och en suffix för filnamnet. I exemplet nedan skapas en temporär fil med prefixet "temp" och suffixet "txt":

```Gleam
let temp_file = File.temp("temp", "txt")
```
När koden körs kommer en temporär fil att skapas med ett unikt nummer för att undvika dubbletter. Du kan sedan använda filen för att läsa eller skriva data och när du är klar kan du radera den genom att använda funktionen `File.remove`:

```Gleam
File.remove(temp_file)
```

# Djupdykning
Att använda tillfälliga filer är ett vanligt sätt att hantera data som inte behövs för framtida bruk eller som inte är nödvändigt att permanent spara. Det är även ett sätt att skydda känslig data från att lagras permanent.

Ett alternativ till att skapa en temporär fil är att skapa en temporär strängvariabel och spara data i den istället. Detta är dock inte alltid det bästa alternativet eftersom det kan innebära en större minnesbelastning.

I Gleam implementeras skapandet av en tillfällig fil genom att använda systemanropet `mkstemp` som finns i de flesta operativsystem.

# Se även
- [Gleam-dokumentationen för File-modulen](https://gleam.run/libraries/file.html)
- [Wikipedia-artikeln om tillfälliga filer](https://en.wikipedia.org/wiki/Temporary_file)