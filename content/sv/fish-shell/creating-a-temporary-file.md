---
title:    "Fish Shell: Skapa en tillfällig fil"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Varför
Om du någonsin har programmerat i Fish Shell har du kanske behövt skapa temporära filer för att utföra vissa uppgifter. Dessa typer av filer kan vara användbara för att tillfälligt lagra data eller för att upprätta en kommunikationskanal mellan olika delar av din kod. I den här blogginlägget kommer vi att diskutera varför det är viktigt att kunna skapa temporära filer i Fish Shell och hur du kan göra det.

## Hur man gör
Skapandet av en temporär fil i Fish Shell är enkelt. Du kan använda kommandot `mktemp` för att skapa en unik, temporär fil. Enheterna som skapas av `mktemp` är tillfälliga och kommer att raderas när de inte längre behövs. Här är ett exempel på hur du kan skapa en temporär fil och skriva något in i den:
```Fish Shell
tempfil = (mktemp)
echo "Det här är en temporär fil" > $tempfil
```

Om du vill att din temporära fil ska ha ett specifikt prefix kan du använda `-p` flaggan. Detta kan vara användbart om du behöver skapa flera temporära filer och vill kunna särskilja dem baserat på deras namn. Här är ett exempel på hur du kan använda `-p` flaggan:
```Fish Shell
tempfil = (mktemp -p prefix_)
```

## Djupdykning
När du skapar en temporär fil skapas den på en slumpmässig plats på din dator och får ett unikt namn för att försäkra att den inte kolliderar med några befintliga filer. Du kan använda `echo $tempfil` för att se var filen har skapats och dess namn. Du kan också använda `rm $tempfil` för att radera filen när du är klar med den.

En annan användbar funktion i samband med temporära filer är att du kan ge dem olika tillgänglighetsnivåer genom att använda `mkfifo`. Detta låter dig skapa en temporär fil som fungerar som en pipeline, vilket innebär att du kan skicka data från en process till en annan. Detta kan vara användbart för att dela data mellan olika delar av din kod.

## Se också
Här är några andra resurser för att hjälpa dig lära dig mer om temporära filer i Fish Shell:

- [Official Fish Shell dokumentation för mktemp](https://fishshell.com/docs/current/cmds/mktemp.html)
- [Unix-tips: "Enkla temporära filer i Fish Shell"](https://www.unixtips.net/en/simple-temporary-files-in-fish-shell/)