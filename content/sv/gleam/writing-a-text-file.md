---
title:                "Gleam: Skriva en textfil"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

##Varför
Att skriva en textfil är en grundläggande del av programmering och något som alla utvecklare behöver kunna. Det är ett sätt att lagra information som kan användas av ett program.

##Så här gör du
För att skriva en textfil i Gleam, använd dig av "```Gleam filnamn.txt```" kodblocket. Du kan också lägga till innehåll i filen genom att använda "```Gleam filnamn.txt med innehåll```". Sedan kan du använda "```Gleam filnamn.txt läsa```" för att läsa innehållet i filen. 

##Djupdykning
När du skriver en textfil i Gleam, är det viktigt att veta hur man hanterar fel och exceptioner. Genom att använda "try-catch" block kan du hantera misstag som kan uppstå under skrivningsprocessen. Du bör också veta hur man stänger en fil efter att du har skrivit i den, genom att använda "close" funktionen. 

##Se även
- "Skriva till en textfil i Gleam" (https://gleam.run/basics/writing-to-files/)
- "Guide till Gleam" (https://gleam.run/getting-started/introduction/)