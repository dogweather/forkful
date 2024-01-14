---
title:    "Gleam: Skapa en temporär fil"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Varför

Att skapa en temporär fil kan vara användbart när man behöver tillfälligt lagra data under körningen av ett Gleam-program. Det kan också vara ett sätt att hantera filer som snabbt behöver tas bort eller ersättas.

## Så här gör du

För att skapa en temporär fil i Gleam, kan man använda funktionen `File.temp_file` och tilldela den till en variabel. Sedan kan man öppna och manipulera filen med hjälp av standardfunktioner för filhantering.

```Gleam
let temp_file = File.temp_file()
File.write(temp_file, "Detta är en temporär fil.")
let content = File.read(temp_file)
io.println(content)
```

Output:

```
Detta är en temporär fil.
```

Det är viktigt att notera att filen kommer att automatiskt raderas när programmet körs klart. Om man vill behålla filen längre, kan man ange en tidsintervall som filen ska finnas kvar i.

## Djupdykning

Vid skapandet av en temporär fil, kommer Gleam att skapa en unik filnamn baserat på en kryptografiskt säker sträng. Detta för att undvika eventuella kollisioner med andra temporära filer. Man kan också ange en annan sökväg än standarden om man vill ha filen i ett annat mapp.

## Se även

- [Dokumentation för File-modulen](https://gleam.run/modules/file/)
- ["Gleaming Bits" - en Gleam-programmeringsblogg](https://gleamingbits.com/)