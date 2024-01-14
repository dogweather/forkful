---
title:                "Bash: Läsning av en textfil"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa textfiler är en viktig del i Bash-programmering eftersom det låter dig läsa, behandla och använda informationen från externa filer i dina skript.

## Så här gör du

För att läsa en textfil i Bash, använder du kommandot `cat` följt av filnamnet som du vill läsa. Till exempel:

```Bash
cat filnamn.txt
```

Detta kommer att skriva ut innehållet i textfilen direkt i terminalen. Om du vill spara innehållet i en variabel för att använda i ditt skript, kan du använda följande syntax:

```Bash
variabel=$(cat filnamn.txt)
```

Nu kan du använda variabeln `variabel` för att behandla innehållet i filen.

Om du bara vill läsa en viss del av filinnehållet, kan du använda kommandot`head` eller `tail` för att skriva ut de första eller sista raderna i filen. Till exempel:

```Bash
head -n 5 filnamn.txt # skriver ut de första 5 raderna i filen
tail -n 3 filnamn.txt # skriver ut de sista 3 raderna i filen
```

Du kan också använda kommandot `grep` för att söka efter ett specifikt ord eller mönster i filen, och sedan använda resultatet i ditt skript. Till exempel:

```Bash
variabel=$(grep "ord" filnamn.txt) # sparar de rader som innehåller ordet "ord" i variabeln
```

## Djupdykning

När du läser textfiler i Bash, är det viktigt att ha kunskap om olika teckenkodningar och om hur kommandon som `cat` och `grep` hanterar specialtecken och linjeslut. Det är också bra att känna till möjligheterna att hantera och manipulera innehållet i en textfil, såsom att ersätta delar av texten eller sortera innehållet.

## Se även

- [Bash-kommandon för filhantering](https://www.dataskolan.net/bash-kommandon-for-filhantering/)
- [En introduktion till Bash-programmering](https://rikardlinde.se/blog/2020/01/03/en_introduktion_till_bash_programmering/)
- [Guide till kodning i Bash](https://help.ubuntu.com/community/Beginners/BASHscripting)