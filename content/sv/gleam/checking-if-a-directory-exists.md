---
title:                "Gleam: Kontrollera om en katalog finns"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför
Att kontrollera om en mapp finns är en vanlig uppgift vid programmering. Oavsett om du behöver skapa en ny mapp eller bara vill kontrollera om en viss mapp redan finns, är det viktigt att veta hur man effektivt kan utföra detta i Gleam.

## Hur man gör
För att kontrollera om en mapp finns i Gleam, kan du använda funktionen `File.exists?`. Den tar en sökväg (path) som argument och returnerar en `Bool` som indikerar om sökvägen leder till en mapp eller inte. Om sökvägen är giltig men inte leder till en mapp, kommer funktionen returnera `false`.

```Gleam
// Skapa en sökväg till en mapp
let path = File.join(["mapp1", "mapp2"])

// Kontrollera om mappen finns
let exists = File.exists?(path)

if exists {
    // Om mappen finns, skriv ut ett meddelande
    io.println("Mappen finns redan!")
} else {
    // Om mappen inte finns, skapa den
    File.mkdir(path)
}
```

Om vi skulle köra koden ovan och mappen `mapp1/mapp2` redan finns, skulle konsolen skriva ut `Mappen finns redan!`. Om mappen inte finns, skulle den skapas och konsolen skulle inte ge något meddelande.

## Fördjupning
När vi använder funktionen `File.exists?` i Gleam, använder vi oss faktiskt av en annan funktion vid namn `std.fs.exists`. Denna funktion är en inbyggd funktion i Gleam-kärnmodulen `std.fs` och är ansvarig för att kontrollera om en fil eller mapp finns. Koden nedan är ett exempel på hur `std.fs.exists` fungerar i bakgrunden.

```Gleam
pub fn exists(path: String) -> Bool {
    match File.metadata(path) {
        Ok(metadata) -> Ok(match metadata.type() {
            File.Type.Directory -> true
            _ -> false
        })
        Err(_) -> false
    }
}
```

Som vi kan se, undersöker denna funktion metadata för sökvägen och returnerar `true` om det är en mapp. Detta används sedan i `File.exists?` för att ge tillbaka resultatet. Det är viktigt att notera att denna process kan variera beroende på vilket operativsystem och filsystem man använder.

## Se även
- [std.fs.exists documentation](https://gleam.run/modules/std/fs.html#fn-exists)
- [Gleam tutorial on file and directory manipulation](https://gleam.run/book/tutorials/files.html)