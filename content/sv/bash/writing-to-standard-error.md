---
title:                "Skriva till standardfel"
html_title:           "Bash: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error är en viktig del av Bash-programmering eftersom det ger en bra sätt att hantera fel och avvikelser i dina skript. Det är ett kraftfullt verktyg för att felsöka och förbättra dina skript.

## Så här gör du

För att skriva till standard error i Bash, används kommandot `>&2` tillsammans med `echo`. Här är ett exempel:

```Bash
echo "Det här är ett felmeddelande" >&2
```

Detta kommer att skriva ut "Det här är ett felmeddelande" till standard error istället för standard output. Det är viktigt att använda `>&2` efter `echo` för att se till att meddelandet faktiskt skrivs ut till standard error och inte standard output. Annars kommer det inte att fungera som förväntat.

Om du vill spara ditt felmeddelande till en fil istället för att visa det på skärmen, kan du använda `>>` för att lägga till innehållet till en befintlig fil eller `>` för att skriva över innehållet i en befintlig fil. Här är några exempel:

```Bash
echo "Fel: Försöker ansluta till en ogiltig host" >&2 >> error.log
echo "Fel: Filen finns inte" >&2 > error.log
```

Dessa kommandon kommer att skriva felmeddelandena till filen `error.log` istället för att visa dem på skärmen.

## Djupdykning

Att skriva till standard error är särskilt användbart när du skapar skript som ska köras automatiskt eller som en del av ett annat program. Genom att skriva till standard error kan du enkelt se om ditt skript har stött på några problem eller fel. På så sätt kan du snabbt hitta och åtgärda eventuella problem utan att behöva gå igenom hela skriptet för att leta efter fel.

En annan fördel med att skriva till standard error är att det är enkelt att separera felmeddelanden från annan output. Om du till exempel använder `grep` för att filtrera ut specifikt innehåll, kan du undvika att felmeddelanden också filtreras ut genom att skriva dem till standard error istället för standard output.

## Se även

- [Bash Guide](https://linuxconfig.org/bash-scripting-tutorial-for-beginners) av LinuxConfig.org
- [Bash Tutorial](https://ryanstutorials.net/bash-scripting-tutorial/) av Ryan's Tutorials
- [Official Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html) av GNU Project