---
title:                "Elm: Skriva en textfil"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

#Varför

Att skriva en textfil kan vara ett användbart sätt att organisera och spara information. I Elm, kan textfiler användas för att lagra data, konfigurationsfiler och även källkod.

#Så här gör du

För att skapa en textfil i Elm, används funktionen `File.write`. Nedan finns ett exempel på hur man skapar en textfil med innehållet "Hej världen":

```Elm
import File

File.write "hello.txt" "Hej världen"
```

När detta körs, kommer en ny fil med namnet "hello.txt" att skapas med texten "Hej världen" som innehåll. Om filen redan finns, kommer den gamla filen att skrivas över.

För att läsa en befintlig textfil, används istället funktionen `File.read`. Nedan finns ett exempel på hur man läser innehållet i en textfil och skriver ut det i konsolen:

```Elm
import File
import Task

-- En hjälpfunktion för att skriva ut resultatet
printResult : Result File.Error String -> Task x ()
printResult result =
    case result of
        Ok text ->
            text
                |> Debug.log "Innehåll i filen:"

        Err error ->
            error
                |> Debug.log "Ett fel uppstod:"

-- Läser innehållet från filen "hello.txt"
Task.attempt printResult (File.read "hello.txt")
```

Detta kommer att skriva ut "Innehåll i filen: Hej världen" i konsolen. Om filen inte finns, kommer det istället att skriva ut "Ett fel uppstod: FileNotFound".

#Djupdykning

När man skriver en textfil i Elm, kan man också använda sig av fält och registervärden för att organisera och strukturera informationen. Detta är särskilt användbart när man ska lagra komplexa datastrukturer.

Man kan också använda sig av funktioner som `File.append` för att lägga till innehåll i en befintlig textfil istället för att skriva över den.

#Se även

- [Officiell dokumentation för Elm filhantering](https://package.elm-lang.org/packages/elm/file/latest/) 
- [Samtliga Elm blogginlägg på swedishelm.org](https://www.swedishelm.org/tag/elm/)