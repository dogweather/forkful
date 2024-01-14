---
title:                "Bash: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför
Att kunna läsa och hantera textfiler är en viktig färdighet inom Bash-programmering. Genom att förstå hur man läser textfiler kan man manipulera och använda data på ett effektivt sätt.

## Hur man gör det
Läsning av textfiler i Bash kan åstadkommas genom användning av kommandot "cat". Detta kommando läser innehållet på en textfil och skriver ut det till standardutdata. Ett enkelt exempel skulle vara:

```Bash
cat min_textfil.txt
```

Detta skulle skriva ut allt innehåll i filen "min_textfil.txt" till skärmen. Om man istället vill spara resultatet från en läsning i en variabel, kan man använda följande kod:

```Bash
variabel=$(cat min_textfil.txt)
echo "$variabel"
```

Här läser vi in filens innehåll och lagrar det i variabeln "variabel", som sedan skrivs ut med hjälp av "echo"-kommandot. Detta är bara ett enkelt exempel på hur man kan läsa en textfil, men det finns många fler sätt att hantera data från en fil.

## Djupdykning
För att kunna läsa och hantera en textfil på ett mer detaljerat sätt, kan man använda sig av olika kommandon inom Bash. Ett användbart kommando är "grep", som används för att söka efter specifika rader eller ord i en fil. Till exempel kan man använda:

```Bash
grep "sökord" min_textfil.txt
```

Detta kommer att söka igenom filen "min_textfil.txt" och skriva ut alla rader som innehåller det angivna sökordet.

Man kan också använda sig av "sed" för att manipulera innehållet i en textfil. Med hjälp av olika sed-uttryck kan man till exempel byta ut specifika ord eller ta bort rader från filen. Det finns många fler kommandon och tekniker man kan använda för att läsa och hantera textfiler i Bash, det viktigaste är att förstå grundläggande koncept och veta vilka kommandon som är tillgängliga.

## Se även
- [Bash Guide for Beginners](https://opensource.com/article/19/10/bash-scripting-guide)
- [Bash Text Processing Commands](https://opensource.com/article/20/9/bash-text-processing)
- [Unix Text Processing Commands](https://www.tutorialspoint.com/unix/unix-text-processing.htm)