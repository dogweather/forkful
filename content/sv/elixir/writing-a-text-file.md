---
title:    "Elixir: Skriva en textfil"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Varför

Att skriva en textfil är en grundläggande aspekt av programmering och Elixir gör detta enklare än någonsin. Genom att lära dig hur man skriver en textfil kan du spara information och data för senare åtkomst och användning i dina Elixir program.

## Hur man gör det

För att börja skriva en textfil i Elixir behöver du först skapa en ny fil på din dator och ge den ett namn med .txt-tillägget. För att skriva till denna fil använder du funktionen `File.write/2` och anger filens namn som första argument och innehållet du vill skriva som andra argument. Till exempel:

```
Elixir
File.write("ny_fli.txt", "Det här är ett exempel på en textfil som skrivs med Elixir.")
```
Detta kommer att skapa en ny fil med namnet "ny_fil.txt" och skriva innehållet i filen som texten "Det här är ett exempel på en textfil som skrivs med Elixir."

## Djupdykning

När du skriver en textfil med Elixir finns det några användbara argument du kan använda för att anpassa din fil. Du kan till exempel ange options för att välja om du vill skriva över befintliga filer eller om du vill lägga till innehållet i slutet av filen istället för i början. Du kan också använda `File.write!/2` för ett mer detaljerat felmeddelande om något går fel vid skrivning av filen.

## Se även

- [Officiell Elixir Dokumentation för textfilhantering](https://hexdocs.pm/elixir/File.html#write/2)
- [En guide om hur man skriver textfiler med Elixir](https://www.thebookofjoel.com/writing-files-elixir)
- [Ett StackOverflow inlägg om att läsa och skriva filer med Elixir](https://stackoverflow.com/questions/7624354/read-write-files-in-elixir)